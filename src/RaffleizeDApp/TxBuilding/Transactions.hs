module RaffleizeDApp.TxBuilding.Transactions where

import Control.Monad.Reader
import Data.Maybe (catMaybes)
import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.Examples.Limbo (addRefScript')
import GeniusYield.GYConfig
import GeniusYield.Imports (IsString (..))
import GeniusYield.TxBuilder
import GeniusYield.Types
import GeniusYield.Types.Key.Class
import PlutusLedgerApi.V1 qualified
import PlutusLedgerApi.V1.Value (AssetClass)
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes (TicketInfo)
import RaffleizeDApp.Tests.UnitTests (greenColorString)
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- *  Queries

------------------------------------------------------------------------------------------------

queryGetAddressFromSkey :: (MonadIO m, MonadReader ProviderCtx m) => GYPaymentSigningKey -> m GYAddress
queryGetAddressFromSkey skey = do
  nid <- asks (cfgNetworkId . ctxCoreCfg)
  runQuery $ do
    let pub_key = paymentVerificationKey skey
        pub_key_hash = pubKeyHash pub_key
        address = addressFromPubKeyHash nid pub_key_hash
    return address

queryGetAddressFromSkeyFile :: (MonadIO m, MonadReader ProviderCtx m) => FilePath -> m ()
queryGetAddressFromSkeyFile skey_file = do
  skey <- liftIO $ readPaymentSigningKey skey_file
  addr <- queryGetAddressFromSkey skey
  liftIO $ printf "Address: %s" (show addr)

queryGetUTxOs :: (MonadIO m, MonadReader ProviderCtx m) => GYAddress -> m GYUTxOs
queryGetUTxOs addr = do
  providers <- asks ctxProviders
  liftIO $ gyQueryUtxosAtAddress providers addr Nothing

queryRaffleizeValidatorUTxOs :: (MonadIO m, MonadReader ProviderCtx m) => m GYUTxOs
queryRaffleizeValidatorUTxOs = do
  gyValidatorAddressGY <- runQuery $ scriptAddress raffleizeValidatorGY
  queryGetUTxOs gyValidatorAddressGY

-- | FILTER ONLY VALID UTXOS BASED ON EXISTANCE OF A RAFFLE STATE TOKEN
queryRaffles :: (MonadIO m, MonadReader ProviderCtx m) => m [(RaffleStateData, PlutusLedgerApi.V1.Value)]
queryRaffles = do
  allUTxOs <- queryRaffleizeValidatorUTxOs
  let validUTxOs = filterUTxOs hasValidRefToken allUTxOs
  let raffles = mapMaybe rsdAndValueFromUTxO (utxosToList validUTxOs)
  return raffles

-- | FILTER ONLY VALID UTXOS BASED ON EXISTANCE OF A RAFFLE STATE TOKEN
queryRafflesInfos :: (MonadIO m, MonadReader ProviderCtx m) => m [RaffleInfo]
queryRafflesInfos = do
  allUTxOs <- queryRaffleizeValidatorUTxOs
  let validUTxOs = filterUTxOs hasValidRefToken allUTxOs
  runQuery $ catMaybes <$> mapM lookupRaffleInfoAtUTxO (utxosToList validUTxOs)

-- | ---
queryRafflesInfosByRefAC :: (MonadIO m, MonadReader ProviderCtx m) => [AssetClass] -> m [RaffleInfo]
queryRafflesInfosByRefAC raffleRefACs = do
  runQuery $ lookupRaffleInfosByACs raffleRefACs

queryMyRaffles :: (MonadIO m, MonadReader ProviderCtx m) => GYAddress -> m [RaffleInfo]
queryMyRaffles addr = do
  gyVal <- runQuery $ queryBalance addr
  let pVal = valueToPlutus gyVal
  let raffleizeUserTokens = getMyRaffleizeUserTokensFromValue pVal
  queryRafflesInfosByRefAC raffleizeUserTokens

-----------------
-----------------
-----------------

queryTicketInfosByUserAC :: (MonadIO m, MonadReader ProviderCtx m) => [AssetClass] -> m [TicketInfo]
queryTicketInfosByUserAC userACs = do
  runQuery $ lookupTicketInfosByACs userACs

queryMyTickets :: (MonadIO m, MonadReader ProviderCtx m) => GYAddress -> m [TicketInfo]
queryMyTickets addr = do
  gyVal <- runQuery $ queryBalance addr
  let pVal = valueToPlutus gyVal
  let raffleizeUserTokens = getMyRaffleizeUserTokensFromValue pVal
  queryTicketInfosByUserAC raffleizeUserTokens

-----------------
-----------------

-- | This function receives a 'RaffleizeInteraction' and a 'RaffleizeTxBuildingContext' and builds the coresponding transaction.
buildRaffleizeTxBody :: (MonadIO m, MonadReader ProviderCtx m) => RaffleizeInteraction -> RaffleizeTxBuildingContext -> m GYTxBody
buildRaffleizeTxBody raffleizeInteraction txb = do
  liftIO $ printf (greenColorString "Building rafllize transaction body...")
  runReader (interactionToTxBody raffleizeInteraction) txb

-----------------
-----------------
-----------------
-----------------
-----------------

submitTxAndWaitForConfirmation :: (MonadIO m, MonadReader ProviderCtx m) => GYTx -> m GYTxOutRef
submitTxAndWaitForConfirmation gyTx = do
  ctxProviders <- asks ctxProviders
  gyTxId <- liftIO $ gySubmitTx ctxProviders gyTx
  liftIO $ printf (greenColorString "Submitted transaction: %s\n Waiting for confirmation ...") gyTxId
  let txId = txIdToApi gyTxId
  liftIO $ gyAwaitTxConfirmed ctxProviders (GYAwaitTxParameters 30 10000000 1) gyTxId
  let txOutRef = txOutRefFromApiTxIdIx txId (wordToApiIx 0)
  return txOutRef

submitTxBodyAndWaitForConfirmation :: (ToShelleyWitnessSigningKey a, MonadIO m, MonadReader ProviderCtx m) => a -> GYTxBody -> m GYTxOutRef
submitTxBodyAndWaitForConfirmation skey txBody = do
  ctxProviders <- asks ctxProviders
  gyTxId <- liftIO $ gySubmitTx ctxProviders $ signGYTxBody txBody [skey]
  liftIO $ printf (greenColorString "Built, signed and submitted transaction: %s\n Waiting for confirmation ...") gyTxId
  let txId = txIdToApi gyTxId
  liftIO $ gyAwaitTxConfirmed ctxProviders (GYAwaitTxParameters 30 10000000 1) gyTxId
  let txOutRef = txOutRefFromApiTxIdIx txId (wordToApiIx 0)
  return txOutRef

------------------------------------------------------------------------------------------------

-- *  Mint Test Tokens Transactions

------------------------------------------------------------------------------------------------

-- | Build a transaction for minting test tokens
buildMintTestTokensTx :: (MonadIO m, MonadReader ProviderCtx m) => GYPaymentSigningKey -> String -> Integer -> m GYTxBody
buildMintTestTokensTx skey tn amount = do
  my_addr <- queryGetAddressFromSkey skey
  runTxI (UserAddresses [my_addr] my_addr Nothing) $ snd <$> mintTestTokens (fromString tn) (fromInteger amount)

mintTestTokensTransaction :: (MonadIO m, MonadReader ProviderCtx m) => GYPaymentSigningKey -> String -> Integer -> m GYTxOutRef
mintTestTokensTransaction skey tn amount = do
  submitTxBodyAndWaitForConfirmation skey =<< buildMintTestTokensTx skey tn amount

------------------------------------------------------------------------------------------------

-- *  Deploy Reference Scripts Transactions

------------------------------------------------------------------------------------------------

deployReferenceScriptTransaction :: (MonadIO m, MonadReader ProviderCtx m) => GYPaymentSigningKey -> GYScript 'PlutusV2 -> m GYTxOutRef
deployReferenceScriptTransaction skey script = do
  submitTxBodyAndWaitForConfirmation skey =<< do
    my_addr <- queryGetAddressFromSkey skey
    runTxI (UserAddresses [my_addr] my_addr Nothing) $ addRefScript' script

deployRaffleizeValidators :: (MonadIO m, MonadReader ProviderCtx m) => GYPaymentSigningKey -> m RaffleizeTxBuildingContext
deployRaffleizeValidators skey = do
  raffleValidatorRef <- deployReferenceScriptTransaction skey (validatorToScript raffleizeValidatorGY)
  ticketValidatoRef <- deployReferenceScriptTransaction skey (validatorToScript ticketValidatorGY)
  return $ RaffleizeTxBuildingContext raffleValidatorRef ticketValidatoRef
