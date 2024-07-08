module RaffleizeDApp.TUI.Utils where

import Data.Aeson
import Data.Either.Extra (eitherToMaybe)
import Data.Text qualified
import Data.Text.IO qualified
import GeniusYield.Types
import RaffleizeDApp.Utils
import System.Directory.Extra

readMnemonicFile :: FilePath -> IO (Maybe GYExtendedPaymentSigningKey)
readMnemonicFile path = do
  putStrLn $ yellowColorString $ "Mnemonic phrase at " <> show path
  fileExist <- doesFileExist path
  if fileExist
    then do
      putStrLn "Found"
      readMnemonic <$> Data.Text.IO.readFile path
    else do
      putStrLn $ show path <> " not found"
      return Nothing

isValidMnemonic :: Text -> Bool
isValidMnemonic = isRight . walletKeysFromMnemonic . Data.Text.words

readMnemonic :: Text -> Maybe GYExtendedPaymentSigningKey
readMnemonic content = eitherToMaybe $ walletKeysToExtendedPaymentSigningKey <$> walletKeysFromMnemonic (Data.Text.words content)

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
