module RaffleizeDApp.TUI.Actions where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.String
import GeniusYield.Types
import PlutusLedgerApi.V1 qualified
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.TUI.Utils
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Transactions

deployValidators :: IO ()
deployValidators = do
  skey <- fromMaybe (error "Skey File Missing") <$> readPaymentKeyFile operationSkeyFilePath
  let msg = "Deploying validators"
  liftIO $ print msg
  validators <- runContextWithCfgProviders (fromString msg) $ deployRaffleizeValidators skey
  liftIO $ B.writeFile raffleizeValidatorsConfig (encode . toJSON $ validators)

getAdminAddress :: GYPaymentSigningKey -> IO GYAddress
getAdminAddress skey = runContextWithCfgProviders "get addmin address" $ queryGetAddressFromSkey skey

getAddrUTxOs :: GYAddress -> IO GYUTxOs
getAddrUTxOs addr = runContextWithCfgProviders "get addmin utxos" $ queryGetUTxOs addr

getAdaBalance :: GYUTxOs -> Ada
getAdaBalance = fromValue . getValueBalance

getValueBalance :: GYUTxOs -> PlutusLedgerApi.V1.Value
getValueBalance = valueToPlutus . foldMapUTxOs utxoValue

getAddressAndValue :: GYPaymentSigningKey -> IO (GYAddress, PlutusLedgerApi.V1.Value)
getAddressAndValue skey = do
  addr <- getAdminAddress skey
  utxos <- getAddrUTxOs addr
  return (addr, getValueBalance utxos)

mintTestTokens :: GYPaymentSigningKey -> String -> Integer -> IO Text
mintTestTokens skey tn amount = do
  let msg = "Minting test tokens"
  liftIO $ print msg
  r <- runContextWithCfgProviders (fromString msg) $ mintTestTokensTransaction skey tn amount
  return $ showTxOutRef r

createRaffle :: GYPaymentSigningKey -> RaffleConfig -> IO Text
createRaffle skey raffle = do
  print emptyString
  print raffle
  let msg = "Create raffle"
  liftIO $ print msg
  r <- runContextWithCfgProviders (fromString msg) $ createRaffleTransaction skey raffle
  return $ showTxOutRef r
