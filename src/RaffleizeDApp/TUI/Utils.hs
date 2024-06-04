module RaffleizeDApp.TUI.Utils where

import Data.Aeson
import GeniusYield.Types
import System.Directory.Extra

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


