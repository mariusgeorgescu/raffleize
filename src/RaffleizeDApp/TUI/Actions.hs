module RaffleizeDApp.TUI.Actions where

import Data.Aeson

import RaffleizeDApp.TxBuilding.Context

import RaffleizeDApp.TxBuilding.Transactions

import Control.Monad.IO.Class
import Data.Csv (toField)
import Data.ByteString.Lazy qualified as B
import Data.String (fromString)
import GeniusYield.Types (Ada, GYAddress, GYPaymentSigningKey, GYUTxO (utxoValue), GYUTxOs, foldMapUTxOs, fromValue, valueToPlutus)
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
  validators <- runContextWithCfgProviders "deploy raffle validator" $ deployRaffleizeValidators skey
  liftIO $ B.writeFile raffleizeValidatorsConfig (encode . toJSON $ validators)

getAdminAddress :: GYPaymentSigningKey -> IO GYAddress
getAdminAddress skey = runContextWithCfgProviders "get addmin address" $ queryGetAddressFromSkey skey

getAdminUTxOs :: GYAddress -> IO GYUTxOs
getAdminUTxOs addr = runContextWithCfgProviders "get addmin utxos" $ queryGetUTxOs addr

getAdaBalance :: GYUTxOs -> Ada
getAdaBalance = fromValue . getValueBalance

getValueBalance :: GYUTxOs -> PlutusLedgerApi.V1.Value
getValueBalance = valueToPlutus . foldMapUTxOs utxoValue

getAddressAndValue :: GYPaymentSigningKey -> IO (GYAddress, PlutusLedgerApi.V1.Value)
getAddressAndValue skey = do
  addr <- getAdminAddress skey
  utxos <- getAdminUTxOs addr
  return (addr, getValueBalance utxos)

mintTestTokens :: GYPaymentSigningKey -> IO String
mintTestTokens skey = do
  let msg = "Minting test tokens"
  liftIO $ print msg
  r <- runContextWithCfgProviders (fromString msg) $ mintTestTokensTransaction skey
  return $ show $ toField r

createRaffle :: GYPaymentSigningKey -> IO String
createRaffle skey = do
  bs <- B.readFile "raffleconfig.json"
  let raffle_config = fromJust $ decode @RaffleConfig bs
  print raffle_config
  let msg = "Create raffle"
  liftIO $ print msg
  r <- runContextWithCfgProviders (fromString msg) $ createRaffleTransaction skey raffle_config
  return $ show $ toField r
