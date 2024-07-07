module RaffleizeDApp.TUI.Utils where

import Data.Aeson
import Data.Either.Extra (eitherToMaybe)
import Data.Text qualified
import Data.Text.IO qualified
import GeniusYield.Types
import RaffleizeDApp.Utils
import System.Directory.Extra

readPaymentKeyFile :: FilePath -> IO (Maybe GYPaymentSigningKey)
readPaymentKeyFile path = do
  putStrLn $ yellowColorString $ "Payment key at " <> show path
  fileExist <- doesFileExist path
  if fileExist
    then do
      putStrLn "Found"
      Just <$> readPaymentSigningKey path
    else do
      putStrLn $ show path <> " not found"
      return Nothing

readMnemonicFile :: FilePath -> IO (Maybe GYExtendedPaymentSigningKey)
readMnemonicFile path = do
  putStrLn $ yellowColorString $ "Mnemonic phrase at " <> show path
  fileExist <- doesFileExist path
  if fileExist
    then do
      putStrLn "Found"
      words <- Data.Text.words <$> Data.Text.IO.readFile path
      return $ eitherToMaybe $ walletKeysToExtendedPaymentSigningKey <$> walletKeysFromMnemonic words
    else do
      putStrLn $ show path <> " not found"
      return Nothing

decodeConfigFile :: FromJSON a => FilePath -> IO (Maybe a)
decodeConfigFile path = do
  putStrLn $ yellowColorString $ "Parsing config file at " <> show path
  fileExist <- doesFileExist path
  if fileExist
    then do
      putStrLn "Found"
      decodeFileStrict path
    else do
      putStrLn $ show path <> " not found"
      return Nothing

generateNewSkey :: FilePath -> IO ()
generateNewSkey path = do
  fileExist <- doesFileExist path
  if fileExist
    then do
      putStrLn $ show path <> " already exists and it was not overwritten"
      return ()
    else do
      skey <- generatePaymentSigningKey
      writePaymentSigningKey path skey
