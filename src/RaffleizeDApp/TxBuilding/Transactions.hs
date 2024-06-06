module RaffleizeDApp.TxBuilding.Transactions where

import Control.Monad.Reader

import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.Examples.Limbo (addRefScript')
import GeniusYield.GYConfig
import GeniusYield.Types
import GeniusYield.Types.Key.Class

import Data.Maybe (catMaybes)
import GeniusYield.Imports (IsString (..))
import GeniusYield.TxBuilder
import PlutusLedgerApi.V1 qualified
import PlutusLedgerApi.V1.Value (AssetClass)
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.Tests.UnitTests (greenColorString)
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions

import RaffleizeDApp.CustomTypes.TicketTypes (TicketInfo)
import RaffleizeDApp.TxBuilding.Lookups (lookupRaffleInfoRefAC, lookupTicketInfoByUserAC)
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators
import PlutusTx.Builtins (blake2b_256)

------------------------------------------------------------------------------------------------

-- *  Queries

------------------------------------------------------------------------------------------------

queryGetAddressFromSkey :: GYPaymentSigningKey -> ReaderT ProviderCtx IO GYAddress
queryGetAddressFromSkey skey = do
  nid <- asks (cfgNetworkId . ctxCoreCfg)
  runQuery $ do
    let pub_key = paymentVerificationKey skey
        pub_key_hash = pubKeyHash pub_key
        address = addressFromPubKeyHash nid pub_key_hash
    return address

queryGetAddressFromSkeyFile :: FilePath -> ReaderT ProviderCtx IO ()
queryGetAddressFromSkeyFile skey_file = do
  skey <- liftIO $ readPaymentSigningKey skey_file
  addr <- queryGetAddressFromSkey skey
  liftIO $ printf "Address: %s" (show addr)

queryGetUTxOs :: GYAddress -> ReaderT ProviderCtx IO GYUTxOs
queryGetUTxOs addr = do
  providers <- asks ctxProviders
  liftIO $ gyQueryUtxosAtAddress providers addr Nothing

queryRaffleizeValidatorUTxOs :: ReaderT ProviderCtx IO GYUTxOs
queryRaffleizeValidatorUTxOs = do
  gyValidatorAddressGY <- runQuery $ scriptAddress raffleizeValidatorGY
  queryGetUTxOs gyValidatorAddressGY

-- | FILTER ONLY VALID UTXOS BASED ON EXISTANCE OF A RAFFLE STATE TOKEN
queryRaffles :: ReaderT ProviderCtx IO [(RaffleStateData, PlutusLedgerApi.V1.Value)]
queryRaffles = do
  allUTxOs <- queryRaffleizeValidatorUTxOs
  let validUTxOs = filterUTxOs hasValidRefToken allUTxOs
  let raffles = mapMaybe rsdAndValueFromUTxO (utxosToList validUTxOs)
  return raffles

-- | FILTER ONLY VALID UTXOS BASED ON EXISTANCE OF A RAFFLE STATE TOKEN
queryRafflesInfos :: ReaderT ProviderCtx IO [RaffleInfo]
queryRafflesInfos = do
  allUTxOs <- queryRaffleizeValidatorUTxOs
  let validUTxOs = filterUTxOs hasValidRefToken allUTxOs
  runQuery $ catMaybes <$> mapM lookupRaffleInfoAtUTxO (utxosToList validUTxOs)

-- | ---
queryRafflesInfosByRefAC :: [AssetClass] -> ReaderT ProviderCtx IO [RaffleInfo]
queryRafflesInfosByRefAC raffleRefACs = do
  runQuery $ catMaybes <$> mapM lookupRaffleInfoRefAC raffleRefACs

queryMyRaffles :: GYAddress -> ReaderT ProviderCtx IO [RaffleInfo]
queryMyRaffles addr = do
  gyVal <- runQuery $ queryBalance addr
  let pVal = valueToPlutus gyVal
  let raffleizeUserTokens = getMyRaffleizeUserTokensFromValue pVal
  queryRafflesInfosByRefAC raffleizeUserTokens

-----------------
-----------------
-----------------

queryTicketInfosByUserAC :: [AssetClass] -> ReaderT ProviderCtx IO [TicketInfo]
queryTicketInfosByUserAC userACs = do
  
  runQuery $ catMaybes <$> mapM lookupTicketInfoByUserAC userACs

queryMyTickets :: GYAddress -> ReaderT ProviderCtx IO [TicketInfo]
queryMyTickets addr = do
  gyVal <- runQuery $ queryBalance addr
  let pVal = valueToPlutus gyVal
  let raffleizeUserTokens = getMyRaffleizeUserTokensFromValue pVal
  queryTicketInfosByUserAC raffleizeUserTokens

-----------------
-----------------
-----------------
-----------------
-----------------

submitTxBody :: (ToShelleyWitnessSigningKey a, MonadIO m, MonadReader ProviderCtx m) => a -> m GYTxBody -> m GYTxId
submitTxBody skey m = do
  txBody <- m
  ctxProviders <- asks ctxProviders
  liftIO $ gySubmitTx ctxProviders $ signGYTxBody txBody [skey]

submitTxBodyAndWaitForConfirmation :: (ToShelleyWitnessSigningKey a, MonadIO m, MonadReader ProviderCtx m) => a -> m GYTxBody -> m GYTxOutRef
submitTxBodyAndWaitForConfirmation skey m = do
  gyTxId <- submitTxBody skey m
  liftIO $ printf (greenColorString "Built, signed and submitted transaction: %s\n Waiting for confirmation ...") gyTxId
  let txId = txIdToApi gyTxId
  ctxProviders <- asks ctxProviders
  liftIO $ gyAwaitTxConfirmed ctxProviders (GYAwaitTxParameters 30 10000000 1) gyTxId
  let txOutRef = txOutRefFromApiTxIdIx txId (wordToApiIx 0)
  return txOutRef

-- | Build a transaction for minting test tokens
buildMintTestTokensTx :: GYPaymentSigningKey -> String -> Integer -> ReaderT ProviderCtx IO GYTxBody
buildMintTestTokensTx skey tn amount = do
  my_addr <- queryGetAddressFromSkey skey
  runTxI (UserAddresses [my_addr] my_addr Nothing) $ snd <$> mintTestTokens (fromString tn) (fromInteger amount)

--------------------------
--------------------------
--------------------------

-- | Build a transaction for creating a new raffle.
buildCreateRaffleTx :: GYPaymentSigningKey -> RaffleConfig -> RaffleizeTxBuildingContext -> ReaderT ProviderCtx IO GYTxBody
buildCreateRaffleTx skey raffleConfiguration validatorsTxOutRefs = do
  my_addr <- queryGetAddressFromSkey skey
  let useraddrs = UserAddresses [my_addr] my_addr Nothing
  let createRaffleInteraction = RaffleizeInteraction Nothing (User (CreateRaffle raffleConfiguration)) useraddrs Nothing
  runReader (interactionToTxBody createRaffleInteraction) validatorsTxOutRefs

createRaffleTransaction :: GYPaymentSigningKey -> RaffleConfig -> RaffleizeTxBuildingContext -> ReaderT ProviderCtx IO GYTxOutRef
createRaffleTransaction skey raffle_config validatorsTxOutRefs = do
  submitTxBodyAndWaitForConfirmation skey $ buildCreateRaffleTx skey raffle_config validatorsTxOutRefs

mintTestTokensTransaction :: GYPaymentSigningKey -> String -> Integer -> ReaderT ProviderCtx IO GYTxOutRef
mintTestTokensTransaction skey tn amount = do
  submitTxBodyAndWaitForConfirmation skey $ buildMintTestTokensTx skey tn amount

------------------------------------------------------------------------------------------------

-- *  Buy Ticket Transaction

------------------------------------------------------------------------------------------------

buildBuyTicketTx :: GYPaymentSigningKey -> String -> AssetClass -> Maybe GYAddress -> RaffleizeTxBuildingContext -> ReaderT ProviderCtx IO GYTxBody
buildBuyTicketTx skey secretString raffleId mRecipient validatorsTxOutRefs = do
  my_addr <- queryGetAddressFromSkey skey
  let useraddrs = UserAddresses [my_addr] my_addr Nothing
  let secretHash =  blake2b_256 $ fromString @BuiltinByteString secretString
  let buyTicketInteraction = RaffleizeInteraction (Just raffleId) (User (BuyTicket secretHash)) useraddrs mRecipient
  runReader (interactionToTxBody buyTicketInteraction) validatorsTxOutRefs

buyTicketTransaction :: GYPaymentSigningKey -> String -> AssetClass -> Maybe GYAddress -> RaffleizeTxBuildingContext -> ReaderT ProviderCtx IO GYTxOutRef
buyTicketTransaction skey secretString raffleId mRecipient validatorsTxOutRefs = do
  submitTxBodyAndWaitForConfirmation skey $ buildBuyTicketTx skey secretString raffleId mRecipient validatorsTxOutRefs

------------------------------------------------------------------------------------------------

-- *  Deploy Reference Scripts Transactions

------------------------------------------------------------------------------------------------

deployReferenceScriptTransaction :: GYPaymentSigningKey -> GYScript 'PlutusV2 -> ReaderT ProviderCtx IO GYTxOutRef
deployReferenceScriptTransaction skey script = do
  submitTxBodyAndWaitForConfirmation skey $ do
    my_addr <- queryGetAddressFromSkey skey
    runTxI (UserAddresses [my_addr] my_addr Nothing) $ addRefScript' script

-- liftIO $ print =<< gyQueryUtxoAtTxOutRef ctxProviders txOutRef

deployRaffleizeValidators :: GYPaymentSigningKey -> ReaderT ProviderCtx IO RaffleizeTxBuildingContext
deployRaffleizeValidators skey = do
  raffleValidatorRef <- deployReferenceScriptTransaction skey (validatorToScript raffleizeValidatorGY)
  ticketValidatoRef <- deployReferenceScriptTransaction skey (validatorToScript ticketValidatorGY)
  return $ RaffleizeTxBuildingContext raffleValidatorRef ticketValidatoRef

-----------------------
-----------------------
-----------------------
-----------------------
-----------------------
