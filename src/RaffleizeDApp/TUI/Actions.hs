module RaffleizeDApp.TUI.Actions where

import Data.Aeson

import RaffleizeDApp.TxBuilding.Context

import RaffleizeDApp.TxBuilding.Transactions

import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as B
import Data.String (fromString)
import Data.Text qualified
import GeniusYield.Types (Ada, GYAddress, GYPaymentSigningKey, GYUTxO (utxoValue), GYUTxOs, foldMapUTxOs, fromValue, showTxOutRef, valueToPlutus)
import PlutusLedgerApi.V1 qualified
import RaffleizeDApp.Constants (
  operationSkeyFilePath,
  raffleizeValidatorsConfig,
 )
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.TUI.Utils

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

mintTestTokens :: GYPaymentSigningKey -> String -> Integer -> IO String
mintTestTokens skey tn amount = do
  let msg = "Minting test tokens"
  liftIO $ print msg
  r <- runContextWithCfgProviders (fromString msg) $ mintTestTokensTransaction skey tn amount
  return $ Data.Text.unpack $ showTxOutRef r

createRaffle :: GYPaymentSigningKey -> IO String
createRaffle skey = do
  bs <- B.readFile "raffleconfig.json"
  let raffle_config = fromJust $ decode @RaffleConfig bs
  print emptyString
  print raffle_config
  let msg = "Create raffle"
  liftIO $ print msg
  r <- runContextWithCfgProviders (fromString msg) $ createRaffleTransaction skey raffle_config
  return $ Data.Text.unpack $ showTxOutRef r
