module TUI.Actions where

import Data.Aeson

import RaffleizeDApp.TxBuilding.Context

import RaffleizeDApp.TxBuilding.Transactions

import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as B

import GeniusYield.Types (GYAddress, GYPaymentSigningKey, GYUTxO (utxoValue), GYUTxOs, GYValue, foldMapUTxOs)
import RaffleizeDApp.Constants (
  operationSkeyFilePath,
  raffleizeValidatorsConfig,
 )
import TUI.Utils

deployValidators :: IO ()
deployValidators = do
  skey <- fromMaybe (error "Skey File Missing") <$> readPaymentKeyFile operationSkeyFilePath
  validators <- runContextWithCfgProviders "deploy raffle validator" $ deployRaffleizeValidators skey
  liftIO $ B.writeFile raffleizeValidatorsConfig (encode . toJSON $ validators)

getAdminAddress :: GYPaymentSigningKey -> IO GYAddress
getAdminAddress skey = runContextWithCfgProviders "get addmin address" $ queryGetAddressFromSkey skey

getAdminUTxOs :: GYAddress -> IO GYUTxOs
getAdminUTxOs addr = runContextWithCfgProviders "get addmin utxos" $ queryGetUTxOs addr

sumValueGYUTxOs :: GYUTxOs -> GYValue
sumValueGYUTxOs = foldMapUTxOs utxoValue

getAddressAndValue :: GYPaymentSigningKey -> IO (GYAddress, GYValue)
getAddressAndValue skey = do
  addr <- getAdminAddress skey
  utxos <- getAdminUTxOs addr
  return (addr, sumValueGYUTxOs utxos)