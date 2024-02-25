module RaffleizeDApp.TxBuilding.Transactions where

import Control.Monad.Reader
import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.GYConfig
import GeniusYield.Types
import GeniusYield.Types.Key.Class
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.RaffleizeOperations (createRaffleTX)

submitTxBody :: (ToShelleyWitnessSigningKey a) => a -> ReaderT Ctx IO GYTxBody -> ReaderT Ctx IO ()
submitTxBody skey m = do
  txBody <- m
  ctxProviders <- asks ctxProviders
  tid <- liftIO $ gySubmitTx ctxProviders $ signGYTxBody txBody [skey]
  liftIO $ printf "submitted tx: %s\n" tid

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

-- | Build a transaction for creating a new raffle.
buildCreateRaffleTx :: GYPaymentSigningKey -> RaffleConfig -> ReaderT Ctx IO GYTxBody
buildCreateRaffleTx skey raffleConfiguration = do
  my_addr <- queryGetAddressFromSkey skey
  runTxI [my_addr] my_addr Nothing (fst <$> createRaffleTX my_addr raffleConfiguration)

-- | Build a transaction for creating a new raffle.
buildMintTestTokensTx :: GYPaymentSigningKey -> ReaderT Ctx IO GYTxBody
buildMintTestTokensTx skey = do
  my_addr <- queryGetAddressFromSkey skey
  runTxI [my_addr] my_addr Nothing $ snd <$> mintTestTokens "teststake" 100

--------------------------
--------------------------
--------------------------

createRaffleTransaction :: GYPaymentSigningKey -> RaffleConfig -> ReaderT Ctx IO ()
createRaffleTransaction skey raffle_config = do
  submitTxBody skey $ buildCreateRaffleTx skey raffle_config

mintTestTokensTransaction :: GYPaymentSigningKey -> ReaderT Ctx IO ()
mintTestTokensTransaction skey = do
  submitTxBody skey $ buildMintTestTokensTx skey

-----------------------
-----------------------
-----------------------
-----------------------
-----------------------
