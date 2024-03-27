module TUI.Actions where

import Data.Aeson

import RaffleizeDApp.TxBuilding.Context

import RaffleizeDApp.TxBuilding.Transactions

import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as B

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
