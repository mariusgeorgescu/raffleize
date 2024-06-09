module RaffleizeDApp.TxBuilding.Transactions where

import Control.Monad.Reader
import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.Examples.Limbo (addRefScript')
import GeniusYield.Imports (IsString (..))
import GeniusYield.Types
import RaffleizeDApp.Tests.UnitTests (greenColorString, yellowColorString)
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions
import RaffleizeDApp.TxBuilding.Validators

-----------------
-----------------
-----------------
-----------------
-----------------

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

-- signSubmitTxBodyAndWaitForConfirmation :: (ToShelleyWitnessSigningKey a, MonadIO m, MonadReader ProviderCtx m) => a -> GYTxBody -> m GYTxOutRef
-- signSubmitTxBodyAndWaitForConfirmation skey txBody = do
--   ctxProviders <- asks ctxProviders
--   gyTxId <- liftIO $ gySubmitTx ctxProviders $ signGYTxBody txBody [skey]
--   liftIO $ printf (greenColorString "Built, signed and submitted transaction: %s\n Waiting for confirmation ...") gyTxId
--   let txId = txIdToApi gyTxId
--   liftIO $ gyAwaitTxConfirmed ctxProviders (GYAwaitTxParameters 30 10000000 1) gyTxId
--   let txOutRef = txOutRefFromApiTxIdIx txId (wordToApiIx 0)
--   return txOutRef

------------------------------------------------------------------------------------------------

-- *  Queries

------------------------------------------------------------------------------------------------

--   providers <- asks ctxProviders
--   liftIO $ gyQueryUtxosAtAddress providers addr Nothing

-- queryRaffleizeValidatorUTxOs :: (MonadIO m, MonadReader ProviderCtx m) => m GYUTxOs
-- queryRaffleizeValidatorUTxOs = do
--   pCtx <- ask
--   gyValidatorAddressGY <- liftIO $ runQuery pCtx (scriptAddress raffleizeValidatorGY)
--   queryGetUTxOs gyValidatorAddressGY

-- -- | FILTER ONLY VALID UTXOS BASED ON EXISTANCE OF A RAFFLE STATE TOKEN
-- queryRaffles :: (MonadIO m, MonadReader ProviderCtx m) => m [(RaffleStateData, PlutusLedgerApi.V1.Value)]
-- queryRaffles = undefined

-- allUTxOs <- queryRaffleizeValidatorUTxOs
-- let validUTxOs = filterUTxOs hasValidRefToken allUTxOs
-- let raffles = mapMaybe rsdAndValueFromUTxO (utxosToList validUTxOs)
-- return raffles

{- | FILTER ONLY VALID UTXOS BASED ON EXISTANCE OF A RAFFLE STATE TOKEN
queryRafflesInfos :: (MonadIO m, MonadReader ProviderCtx m) => m [RaffleInfo]
queryRafflesInfos = undefined
-}

-- allUTxOs <- queryRaffleizeValidatorUTxOs
-- let validUTxOs = filterUTxOs hasValidRefToken allUTxOs
-- liftIO $ catMaybes <$> mapM lookupRaffleInfoAtUTxO (utxosToList validUTxOs)

-- -- | ---
-- queryRafflesInfosByRefAC :: (MonadIO m, MonadReader ProviderCtx m) => [AssetClass] -> m [RaffleInfo]
-- queryRafflesInfosByRefAC raffleRefACs = do
--   lookupRaffleInfosByACs raffleRefACs

--   gyVal <- queryBalance addr
--   let pVal = valueToPlutus gyVal
--   let raffleizeUserTokens = getMyRaffleizeUserTokensFromValue pVal
--   queryRafflesInfosByRefAC raffleizeUserTokens

-- -----------------
-- -----------------
-- -----------------

-- queryTicketInfosByUserAC :: (MonadIO m, MonadReader ProviderCtx m) => [AssetClass] -> m [TicketInfo]
-- queryTicketInfosByUserAC userACs = do
--   lookupTicketInfosByACs userACs

-- gyVal <- queryBalance addr
-- let pVal = valueToPlutus gyVal
-- let raffleizeUserTokens = getMyRaffleizeUserTokensFromValue pVal
-- queryTicketInfosByUserAC raffleizeUserTokens

------------------------------------------------------------------------------------------------

-- *  Mint Test Tokens Transactions

------------------------------------------------------------------------------------------------

{- | This function  'UserAddresses', 'String' representing the token name and an 'Integer' representing the amount,
| and returns the 'GYTxBody' of a transaction minting the test tokens.
-}
mintTestTokensTxBody :: (MonadIO m, MonadReader ProviderCtx m) => UserAddresses -> String -> Integer -> m GYTxBody
mintTestTokensTxBody userAddresses tn amount = do
  providerCtx <- ask
  liftIO $ runTxI providerCtx userAddresses $ snd <$> mintTestTokens (fromString tn) (fromInteger amount)

------------------------------------------------------------------------------

-- *  Deploy Reference Scripts Transactions

------------------------------------------------------------------------------------------------

deployReferenceScriptTxBody :: (MonadIO m, MonadReader ProviderCtx m) => GYScript 'PlutusV2 -> UserAddresses -> m GYTxBody
deployReferenceScriptTxBody script userAddresses = do
  providerCtx <- ask
  liftIO $ runTxI providerCtx userAddresses (addRefScript' script)

deployRaffleizeValidatortTxBody :: (MonadIO m, MonadReader ProviderCtx m) => UserAddresses -> m GYTxBody
deployRaffleizeValidatortTxBody = deployReferenceScriptTxBody (validatorToScript raffleizeValidatorGY)

deployTicketValidatortTxBody :: (MonadIO m, MonadReader ProviderCtx m) => UserAddresses -> m GYTxBody
deployTicketValidatortTxBody = deployReferenceScriptTxBody (validatorToScript raffleizeValidatorGY)

-- deployReferenceScriptTransaction :: (MonadIO m, MonadReader ProviderCtx m) => GYPaymentSigningKey -> GYScript 'PlutusV2 -> m GYTxOutRef
-- deployReferenceScriptTransaction skey script = do
--   pCtx <- ask
--   let nid = (cfgNetworkId . ctxCoreCfg) pCtx
--   let my_addr = addressFromPaymentSigningKey nid skey
--   let userAddresses = UserAddresses [my_addr] my_addr Nothing
--   txBody <- deployReferenceScriptTxBody userAddresses script
--   signSubmitTxBodyAndWaitForConfirmation skey txBody

-- deployRaffleizeValidators :: (MonadIO m, MonadReader ProviderCtx m) => GYPaymentSigningKey -> m RaffleizeTxBuildingContext
-- deployRaffleizeValidators skey = do
--   raffleValidatorRef <- deployReferenceScriptTransaction skey (validatorToScript raffleizeValidatorGY)
--   ticketValidatoRef <- deployReferenceScriptTransaction skey (validatorToScript ticketValidatorGY)
--   return $ RaffleizeTxBuildingContext raffleValidatorRef ticketValidatoRef

-----------------
-- RaffleizeOffchainContext
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

-- class (MonadReader RaffleizeOffchainContext m, MonadIO m) => RaffleizeOffchain m where
--   buildRaffleizeTransaction :: RaffleizeInteraction -> m GYTxBody
--   submitRaffleizeTransaction :: GYTx -> m GYTxOutRef
--   lookupAllRaffles :: m [RaffleInfo]
--   lookupRafflesByUserAddresses :: UserAddresses -> m [RaffleInfo]
--   lookupTicketsByUserAddresses :: UserAddresses -> m [RaffleInfo]