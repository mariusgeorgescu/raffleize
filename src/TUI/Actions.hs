module TUI.Actions where

import Data.Aeson

import RaffleizeDApp.TxBuilding.Context

import RaffleizeDApp.TxBuilding.Transactions

import System.Directory.Extra (doesFileExist)

import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as B
import GeniusYield.Types
import RaffleizeDApp.Constants

deployValidators :: IO ()
deployValidators = do
  skey <- readPaymentSigningKey operationSkeyFilePath
  validators <- runContextWithCfgProviders "deploy raffle validator" $ deployRaffleizeValidators skey
  liftIO $ B.writeFile raffleizeValidatorsConfig (encode . toJSON $ validators)

readPaymentKeyFile :: FilePath -> IO (Maybe GYPaymentSigningKey)
readPaymentKeyFile path = do
  fileExist <- doesFileExist path
  if fileExist
    then Just <$> readPaymentSigningKey path
    else return Nothing

decodeConfigFile :: FromJSON a => FilePath -> IO (Maybe a)
decodeConfigFile path = do
  fileExist <- doesFileExist path
  if fileExist
    then decodeFileStrict path
    else return Nothing

generateNewAdminSkey :: FilePath -> IO ()
generateNewAdminSkey path = do
  fileExist <- doesFileExist path
  if fileExist
    then return ()
    else do
      skey <- generatePaymentSigningKey
      writePaymentSigningKey path skey