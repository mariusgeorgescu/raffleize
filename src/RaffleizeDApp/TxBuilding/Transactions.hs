module RaffleizeDApp.TxBuilding.Transactions where

import Control.Monad.Reader
import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.Imports (IsString (..))
import GeniusYield.Types
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions
import RaffleizeDApp.TxBuilding.Skeletons (addRefScriptSkeleton)
import RaffleizeDApp.TxBuilding.Validators
import RaffleizeDApp.Utils

------------------------------------------------------------------------------------------------

-- * Transaction Building

------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------

-- **  Mint Test Tokens

------------------------------------------------------------------------------------------------

{- | This function  'UserAddresses', 'String' representing the token name and an 'Integer' representing the amount,
| and returns the 'GYTxBody' of a transaction minting the test tokens.
-}
mintTestTokensTxBody :: (MonadIO m, MonadReader ProviderCtx m) => UserAddresses -> String -> Integer -> m GYTxBody
mintTestTokensTxBody userAddresses tn amount = do
  providerCtx <- ask
  liftIO $ runTxI providerCtx userAddresses $ snd <$> mintTestTokens (fromString tn) (fromInteger amount)

------------------------------------------------------------------------------

-- **  Deploy Reference Scripts

------------------------------------------------------------------------------------------------

deployReferenceScriptTxBody :: (MonadIO m, MonadReader ProviderCtx m) => GYScript 'PlutusV3 -> UserAddresses -> m GYTxBody
deployReferenceScriptTxBody script userAddresses = do
  providerCtx <- ask
  liftIO $ runTxI providerCtx userAddresses (addRefScriptSkeleton script)

deployRaffleizeValidatortTxBody :: (MonadIO m, MonadReader ProviderCtx m) => UserAddresses -> m GYTxBody
deployRaffleizeValidatortTxBody = deployReferenceScriptTxBody (validatorToScript raffleizeValidatorGY)

deployTicketValidatortTxBody :: (MonadIO m, MonadReader ProviderCtx m) => UserAddresses -> m GYTxBody
deployTicketValidatortTxBody = deployReferenceScriptTxBody (validatorToScript ticketValidatorGY)

-----------------

-- **  Raffleize Interactions

-----------------

interactionToTxBody :: (MonadReader RaffleizeOffchainContext m, MonadIO m) => RaffleizeInteraction -> m GYTxBody
interactionToTxBody interaction@RaffleizeInteraction {userAddresses} = do
  roc <- ask
  let skeleton = runReader (interactionToTxSkeleton interaction) (raffleizeTxBuildingCtx roc)
  liftIO $ runTxI (providerCtx roc) userAddresses (fst <$> skeleton)

interactionToUnsignedTx :: (MonadReader RaffleizeOffchainContext m, MonadIO m) => RaffleizeInteraction -> m GYTx
interactionToUnsignedTx = (unsignedTx <$>) . interactionToTxBody

interactionToHexEncodedCBOR :: (MonadReader RaffleizeOffchainContext m, MonadIO m) => RaffleizeInteraction -> m String
interactionToHexEncodedCBOR = (txToHex <$>) . interactionToUnsignedTx

------------------------------------------------------------------------------------------------

-- * Transaction Submitting

------------------------------------------------------------------------------------------------
submitTxAndWaitForConfirmation :: (MonadIO m, MonadReader ProviderCtx m) => GYTx -> m GYTxOutRef
submitTxAndWaitForConfirmation gyTx = do
  ctxProviders <- asks ctxProviders
  gyTxId <- liftIO $ gySubmitTx ctxProviders gyTx
  liftIO $ printf (greenColorString "Submitted transaction: \n\t %s") gyTxId
  liftIO $ putStrLn (yellowColorString "Waiting for confirmation ...")
  let txId = txIdToApi gyTxId
  liftIO $ gyAwaitTxConfirmed ctxProviders (GYAwaitTxParameters 30 10000000 1) gyTxId
  let txOutRef = txOutRefFromApiTxIdIx txId (wordToApiIx 0)
  return txOutRef
