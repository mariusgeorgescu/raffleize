module RaffleizeDApp.TxBuilding.Transactions where

import Control.Monad.Reader

import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.Examples.Limbo (addRefScript')
import GeniusYield.GYConfig
import GeniusYield.Types
import GeniusYield.Types.Key.Class

import GeniusYield.TxBuilder
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.Tests.UnitTests (greenColorString)
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions
import RaffleizeDApp.TxBuilding.Lookups (getRaffleDatumAndValue)
import RaffleizeDApp.TxBuilding.Validators (raffleizeValidatorGY, ticketValidatorGY)

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

-- TODO FILTER ONLY VALID UTXOS BASED ON EXISTANCE OF A RAFFLE STATE TOKEN

queryRaffles :: ReaderT ProviderCtx IO [String]
queryRaffles = do
  raffflesUTxOs <- queryRaffleizeValidatorUTxOs
  return $ show <$> utxosToList raffflesUTxOs

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
  liftIO $ gyAwaitTxConfirmed ctxProviders (GYAwaitTxParameters 10 50000000 1) gyTxId
  let txOutRef = txOutRefFromApiTxIdIx txId (wordToApiIx 0)
  return txOutRef

-- | Build a transaction for minting test tokens
buildMintTestTokensTx :: GYPaymentSigningKey -> ReaderT ProviderCtx IO GYTxBody
buildMintTestTokensTx skey = do
  my_addr <- queryGetAddressFromSkey skey
  runTxI (UserAddresses [my_addr] my_addr Nothing) $ snd <$> mintTestTokens "teststake" 100

--------------------------
--------------------------
--------------------------

-- | Build a transaction for creating a new raffle.
buildCreateRaffleTx :: GYPaymentSigningKey -> RaffleConfig -> ReaderT ProviderCtx IO GYTxBody
buildCreateRaffleTx skey raffleConfiguration = do
  my_addr <- queryGetAddressFromSkey skey
  let useraddrs = UserAddresses [my_addr] my_addr Nothing
  let createRaffleInteraction = RaffleizeInteraction Nothing (User (CreateRaffle raffleConfiguration)) useraddrs Nothing
  runReader (interactionToTxBody createRaffleInteraction) undefined

createRaffleTransaction :: GYPaymentSigningKey -> RaffleConfig -> ReaderT ProviderCtx IO GYTxOutRef
createRaffleTransaction skey raffle_config = do
  submitTxBodyAndWaitForConfirmation skey $ buildCreateRaffleTx skey raffle_config

mintTestTokensTransaction :: GYPaymentSigningKey -> ReaderT ProviderCtx IO GYTxOutRef
mintTestTokensTransaction skey = do
  submitTxBodyAndWaitForConfirmation skey $ buildMintTestTokensTx skey

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
