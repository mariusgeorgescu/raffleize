module RaffleizeDApp.TxBuilding.Transactions where

import Control.Monad.Reader

import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.Examples.Limbo (addRefScript')
import GeniusYield.GYConfig
import GeniusYield.Types
import GeniusYield.Types.Key.Class

import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions
import RaffleizeDApp.TxBuilding.Validators (raffleizeValidatorGY, ticketValidatorGY)

------------------------------------------------------------------------------------------------

-- *  Queries

------------------------------------------------------------------------------------------------

queryGetAddressFromSkey :: GYPaymentSigningKey -> ReaderT Ctx IO GYAddress
queryGetAddressFromSkey skey = do
  nid <- asks (cfgNetworkId . ctxCoreCfg)
  runQuery $ do
    let pub_key = paymentVerificationKey skey
        pub_key_hash = pubKeyHash pub_key
        address = addressFromPubKeyHash nid pub_key_hash
    return address

queryGetAddressFromSkeyFile :: FilePath -> ReaderT Ctx IO ()
queryGetAddressFromSkeyFile skey_file = do
  skey <- liftIO $ readPaymentSigningKey skey_file
  addr <- queryGetAddressFromSkey skey
  liftIO $ printf "Address: %s" (show addr)

queryGetUTxOs :: GYAddress -> ReaderT Ctx IO GYUTxOs
queryGetUTxOs addr = do
  providers <- asks ctxProviders
  liftIO $ gyQueryUtxosAtAddress providers addr Nothing

-----------------
-----------------
-----------------
-----------------
-----------------

submitTxBody :: (ToShelleyWitnessSigningKey a, MonadIO m, MonadReader Ctx m) => a -> m GYTxBody -> m ()
submitTxBody skey m = do
  txBody <- m
  ctxProviders <- asks ctxProviders
  tid <- liftIO $ gySubmitTx ctxProviders $ signGYTxBody txBody [skey]
  liftIO $ printf "submitted tx: %s\n" tid

submitTxBody' :: (ToShelleyWitnessSigningKey a, MonadIO m, MonadReader Ctx m) => a -> m GYTxBody -> m GYTxId
submitTxBody' skey m = do
  txBody <- m
  ctxProviders <- asks ctxProviders
  tid <- liftIO $ gySubmitTx ctxProviders $ signGYTxBody txBody [skey]
  liftIO $ printf "submitted tx: %s\n" tid
  return tid

-- | Build a transaction for creating a new raffle.
buildMintTestTokensTx :: GYPaymentSigningKey -> ReaderT Ctx IO GYTxBody
buildMintTestTokensTx skey = do
  my_addr <- queryGetAddressFromSkey skey
  runTxI (UserAddresses [my_addr] my_addr Nothing) $ snd <$> mintTestTokens "teststake" 100

--------------------------
--------------------------
--------------------------

-- | Build a transaction for creating a new raffle.
buildCreateRaffleTx :: GYPaymentSigningKey -> RaffleConfig -> ReaderT Ctx IO GYTxBody
buildCreateRaffleTx skey raffleConfiguration = do
  my_addr <- queryGetAddressFromSkey skey
  let useraddrs = UserAddresses [my_addr] my_addr Nothing
  let createRaffleInteraction = RaffleizeInteraction Nothing (User (CreateRaffle raffleConfiguration)) useraddrs Nothing
  runReader (interactionToTxBody createRaffleInteraction) undefined

createRaffleTransaction :: GYPaymentSigningKey -> RaffleConfig -> ReaderT Ctx IO ()
createRaffleTransaction skey raffle_config = do
  submitTxBody skey $ buildCreateRaffleTx skey raffle_config

mintTestTokensTransaction :: GYPaymentSigningKey -> ReaderT Ctx IO ()
mintTestTokensTransaction skey = do
  submitTxBody skey $ buildMintTestTokensTx skey

------------------------------------------------------------------------------------------------

-- *  Deploy Reference Scripts Transactions

------------------------------------------------------------------------------------------------

deployReferenceScriptTransaction :: GYPaymentSigningKey -> GYScript 'PlutusV2 -> ReaderT Ctx IO GYTxOutRef
deployReferenceScriptTransaction skey script = do
  gyTxId <- submitTxBody' skey $ do
    my_addr <- queryGetAddressFromSkey skey
    runTxI (UserAddresses [my_addr] my_addr Nothing) $ addRefScript' script
  let txId = txIdToApi gyTxId
  ctxProviders <- asks ctxProviders
  liftIO $ gyAwaitTxConfirmed ctxProviders (GYAwaitTxParameters 3 50000000 1) gyTxId
  let txOutRef = txOutRefFromApiTxIdIx txId (wordToApiIx 0)
  -- liftIO $ print =<< gyQueryUtxoAtTxOutRef ctxProviders txOutRef
  return txOutRef

deployRaffleizeValidators :: GYPaymentSigningKey -> ReaderT Ctx IO RaffleizeTxBuildingContext
deployRaffleizeValidators skey = do
  raffleValidatorRef <- deployReferenceScriptTransaction skey (validatorToScript raffleizeValidatorGY)
  ticketValidatoRef <- deployReferenceScriptTransaction skey (validatorToScript ticketValidatorGY)
  return $ RaffleizeTxBuildingContext raffleValidatorRef ticketValidatoRef

-----------------------
-----------------------
-----------------------
-----------------------
-----------------------
