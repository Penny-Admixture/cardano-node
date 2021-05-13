{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Shelley.Run.Governance
  ( ShelleyGovernanceCmdError
  , renderShelleyGovernanceError
  , runGovernanceCmd
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text
import Data.Sequence.Strict (singleton)

import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT, right)

import           Cardano.Api
import           Cardano.Api.Shelley
import           Cardano.Api.Pivo ()

import           Cardano.CLI.Shelley.Key (InputDecodeError, VerificationKeyOrHashOrFile,
                      VerificationKeyOrFile (VerificationKeyFilePath),
                     readVerificationKeyOrHashOrFile, readVerificationKeyOrHashOrTextEnvFile, readVerificationKeyOrFile)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Types

import Cardano.Ledger.Era (Era)

import Shelley.Spec.Ledger.BaseTypes (maybeToStrictMaybe)
import Shelley.Spec.Ledger.PParams (PParamsUpdate, emptyPParamsUpdate, _maxBBSize)
import qualified Shelley.Spec.Ledger.TxBody as Shelley

import qualified Cardano.Ledger.Pivo.Update as Pivo.Update
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP as SIP
import qualified Cardano.Ledger.Pivo.Update.Payload.Implementation as IMP

import qualified Shelley.Spec.Ledger.Keys as Shelley.Keys
import Ouroboros.Consensus.Shelley.Eras (StandardPivo)


data ShelleyGovernanceCmdError
  = ShelleyGovernanceCmdTextEnvReadError !(FileError TextEnvelopeError)
  | ShelleyGovernanceCmdKeyReadError !(FileError InputDecodeError)
  | ShelleyGovernanceCmdTextEnvWriteError !(FileError ())
  | ShelleyGovernanceCmdEmptyUpdateProposalError
  | ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach
      !FilePath
      !Int
      -- ^ Number of stake verification keys
      !Int
      -- ^ Number of reward amounts
  deriving Show

renderShelleyGovernanceError :: ShelleyGovernanceCmdError -> Text
renderShelleyGovernanceError err =
  case err of
    ShelleyGovernanceCmdTextEnvReadError fileErr -> Text.pack (displayError fileErr)
    ShelleyGovernanceCmdKeyReadError fileErr -> Text.pack (displayError fileErr)
    ShelleyGovernanceCmdTextEnvWriteError fileErr -> Text.pack (displayError fileErr)
    -- TODO: The equality check is still not working for empty update proposals.
    ShelleyGovernanceCmdEmptyUpdateProposalError ->
      "Empty update proposals are not allowed"
    ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach fp numVKeys numRwdAmts ->
       "Error creating the MIR certificate at: " <> textShow fp
       <> " The number of staking keys: " <> textShow numVKeys
       <> " and the number of reward amounts: " <> textShow numRwdAmts
       <> " are not equivalent."
  where
    textShow x = Text.pack (show x)


runGovernanceCmd :: GovernanceCmd -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceCmd (GovernanceMIRCertificate mirpot vKeys rewards out) =
  runGovernanceMIRCertificate mirpot vKeys rewards out
runGovernanceCmd (GovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out) =
  runGovernanceGenesisKeyDelegationCertificate genVk genDelegVk vrfVk out
runGovernanceCmd (GovernanceUpdateProposal out eNo genVKeys ppUp) =
  runGovernanceUpdateProposal out eNo genVKeys ppUp
runGovernanceCmd (PivoCmd pivoCmd (OutputFile outFile)) = runPivoCmd pivoCmd
  where
    runPivoCmd (SIP SIPNew { sipAuthorKeyFile, proposalText, proposalVPD }) = do
      StakeVerificationKey (Shelley.Keys.VKey vk)
        <- firstExceptT ShelleyGovernanceCmdKeyReadError . newExceptT
         $ readVerificationKeyOrFile AsStakeKey
         $ VerificationKeyFilePath sipAuthorKeyFile
      let sip = SIP.mkSubmission vk constSalt (mkProposal proposalText proposalVPD)
      -- todo: use 'returnPayload' instead.
      firstExceptT ShelleyGovernanceCmdTextEnvWriteError
        $ newExceptT
        $ writeFileTextEnvelope
            outFile
            Nothing
            (mempty
               { Pivo.Update.sipSubmissions = singleton sip }
             :: Pivo.Update.Payload StandardPivo
            )
    runPivoCmd (SIP SIPReveal { sipRevelatorKeyFile, revealedProposalText, revealedProposalVPD }) = do
      StakeVerificationKey (Shelley.Keys.VKey vk)
        <- firstExceptT ShelleyGovernanceCmdKeyReadError . newExceptT
         $ readVerificationKeyOrFile AsStakeKey
         $ VerificationKeyFilePath sipRevelatorKeyFile
      let revelation =
            SIP.mkRevelation vk constSalt (mkProposal revealedProposalText revealedProposalVPD)
      firstExceptT ShelleyGovernanceCmdTextEnvWriteError
        $ newExceptT
        $ writeFileTextEnvelope
            outFile
            Nothing
            (mempty
               { Pivo.Update.sipRevelations = singleton revelation }
             :: Pivo.Update.Payload StandardPivo
            )
    runPivoCmd (SIP SIPVote { sipVoterKeyFile, votedProposalText, votedProposalVPD }) = do
      vk <- readUpdateKeyFile sipVoterKeyFile
      returnPayload $
        mempty
          { Pivo.Update.sipVotes       = singleton
                                       $ SIP.mkVote @StandardPivo
                                           vk
                                           (SIP._id (mkProposal votedProposalText votedProposalVPD))
                                           SIP.For
          }
    runPivoCmd (IMP IMPCommit { impCommiterKeyFile, impCommitSIPText, impCommitVersion, impCommitNewBBSize }) = do
      vk <- readUpdateKeyFile impCommiterKeyFile
      let ppUpdate = emptyPParamsUpdate { _maxBBSize = maybeToStrictMaybe impCommitNewBBSize }
      returnPayload $
        mempty
          { Pivo.Update.impSubmissions =
              singleton $ IMP.mkSubmission @StandardPivo vk constSalt
                        $ mkImplementation impCommitSIPText impCommitVersion ppUpdate
          }
    runPivoCmd (IMP IMPReveal { impRevelatorKeyFile, impRevelationSIPText, impRevelationVersion, impRevealNewBBSize }) = do
      vk <- readUpdateKeyFile impRevelatorKeyFile
      let ppUpdate = emptyPParamsUpdate { _maxBBSize = maybeToStrictMaybe impRevealNewBBSize }
      returnPayload $
        mempty
          { Pivo.Update.impRevelations =
              singleton $ IMP.mkRevelation @StandardPivo vk constSalt
                        $ mkImplementation impRevelationSIPText impRevelationVersion ppUpdate
          }
    runPivoCmd (IMP IMPVote { impVoterKeyFile, impVotedSIPText, impVotedVersion, impVoteNewBBSize }) = do
      vk <- readUpdateKeyFile impVoterKeyFile
      let ppUpdate = emptyPParamsUpdate { _maxBBSize = maybeToStrictMaybe impVoteNewBBSize }
      returnPayload $
        mempty
          { Pivo.Update.impVotes       =
              singleton $ IMP.mkVote @StandardPivo
                            vk
                            (SIP._id $ mkImplementation impVotedSIPText impVotedVersion ppUpdate)
                            SIP.For
          }
    runPivoCmd (END ENDCmd { endorserKeyFile, endorsedVersion }) = do
      vk <- readUpdateKeyFile endorserKeyFile
      returnPayload $
        mempty
          { Pivo.Update.endorsements =
              singleton $ IMP.mkEndorsement @StandardPivo vk endorsedVersion

          }
    readUpdateKeyFile keyFile = do
      StakeVerificationKey (Shelley.Keys.VKey vk)
        <- firstExceptT ShelleyGovernanceCmdKeyReadError . newExceptT
         $ readVerificationKeyOrFile AsStakeKey
         $ VerificationKeyFilePath keyFile
      return $! vk
    returnPayload updatePayload =
      firstExceptT ShelleyGovernanceCmdTextEnvWriteError
        $ newExceptT
        $ writeFileTextEnvelope outFile Nothing updatePayload

-- | Make an ad-hoc proposal using the given proposal text.
mkProposal
  :: Era era
  => Text
  -> SlotNo
  -- ^ Proposal's voting period duration
  -> SIP.Proposal era
mkProposal text duration = SIP.mkProposal text duration

-- todo: add support for specifying the salt through the command line.
constSalt :: Int
constSalt = 84

-- FIXME: we need to make this function take a voting period duration as
-- specified in the command line for the SIP.
mkImplementation
  :: Era era
  => Text -> Word -> PParamsUpdate era -> IMP.Implementation era
mkImplementation sipText impVersion ppUpdate =
  IMP.mkImplementation
    (SIP.unProposalId $ SIP._id proposal) constVotingPeriodDuration protocol
  where
    proposal = mkProposal sipText constVotingPeriodDuration
    protocol = IMP.mkProtocol impVersion IMP.protocolZero ppUpdate
    -- TODO: add support for specifying the voting period duration
    -- through the command line.
    constVotingPeriodDuration :: SlotNo
    constVotingPeriodDuration = 600

runGovernanceMIRCertificate
  :: Shelley.MIRPot
  -> [VerificationKeyFile]
  -- ^ Stake verification keys
  -> [Lovelace]
  -- ^ Reward amounts
  -> OutputFile
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceMIRCertificate mirPot vKeys rwdAmts (OutputFile oFp) = do
    sCreds <- mapM readStakeKeyToCred vKeys

    checkEqualKeyRewards vKeys rwdAmts

    let mirCert = makeMIRCertificate mirPot (zip sCreds rwdAmts)

    firstExceptT ShelleyGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just mirCertDesc) mirCert
  where
    mirCertDesc :: TextEnvelopeDescr
    mirCertDesc = "Move Instantaneous Rewards Certificate"

    checkEqualKeyRewards :: [VerificationKeyFile] -> [Lovelace] -> ExceptT ShelleyGovernanceCmdError IO ()
    checkEqualKeyRewards keys rwds = do
       let numVKeys = length keys
           numRwdAmts = length rwds
       if numVKeys == numRwdAmts
       then return () else left $ ShelleyGovernanceCmdMIRCertificateKeyRewardMistmach oFp numVKeys numRwdAmts

    readStakeKeyToCred :: VerificationKeyFile -> ExceptT ShelleyGovernanceCmdError IO StakeCredential
    readStakeKeyToCred (VerificationKeyFile stVKey) = do
      stakeVkey <- firstExceptT ShelleyGovernanceCmdTextEnvReadError
        . newExceptT
        $ readFileTextEnvelope (AsVerificationKey AsStakeKey) stVKey
      right . StakeCredentialByKey $ verificationKeyHash stakeVkey

runGovernanceGenesisKeyDelegationCertificate
  :: VerificationKeyOrHashOrFile GenesisKey
  -> VerificationKeyOrHashOrFile GenesisDelegateKey
  -> VerificationKeyOrHashOrFile VrfKey
  -> OutputFile
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceGenesisKeyDelegationCertificate genVkOrHashOrFp
                                             genDelVkOrHashOrFp
                                             vrfVkOrHashOrFp
                                             (OutputFile oFp) = do
    genesisVkHash <- firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisKey genVkOrHashOrFp
    genesisDelVkHash <-firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrTextEnvFile AsGenesisDelegateKey genDelVkOrHashOrFp
    vrfVkHash <- firstExceptT ShelleyGovernanceCmdKeyReadError
      . newExceptT
      $ readVerificationKeyOrHashOrFile AsVrfKey vrfVkOrHashOrFp
    firstExceptT ShelleyGovernanceCmdTextEnvWriteError
      . newExceptT
      $ writeFileTextEnvelope oFp (Just genKeyDelegCertDesc)
      $ makeGenesisKeyDelegationCertificate genesisVkHash genesisDelVkHash vrfVkHash
  where
    genKeyDelegCertDesc :: TextEnvelopeDescr
    genKeyDelegCertDesc = "Genesis Key Delegation Certificate"

runGovernanceUpdateProposal
  :: OutputFile
  -> EpochNo
  -> [VerificationKeyFile]
  -- ^ Genesis verification keys
  -> ProtocolParametersUpdate
  -> ExceptT ShelleyGovernanceCmdError IO ()
runGovernanceUpdateProposal (OutputFile upFile) eNo genVerKeyFiles upPprams = do
    when (upPprams == mempty) $ left ShelleyGovernanceCmdEmptyUpdateProposalError
    genVKeys <- sequence
                  [ firstExceptT ShelleyGovernanceCmdTextEnvReadError . newExceptT $
                      readFileTextEnvelope
                        (AsVerificationKey AsGenesisKey)
                        vkeyFile
                  | VerificationKeyFile vkeyFile <- genVerKeyFiles ]
    let genKeyHashes = map verificationKeyHash genVKeys
        upProp = makeShelleyUpdateProposal upPprams genKeyHashes eNo
    firstExceptT ShelleyGovernanceCmdTextEnvWriteError . newExceptT $
      writeFileTextEnvelope upFile Nothing upProp
