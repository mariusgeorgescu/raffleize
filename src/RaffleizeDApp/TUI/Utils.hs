module RaffleizeDApp.TUI.Utils where

import Data.Aeson
import GeniusYield.Types

import RaffleizeDApp.Tests.UnitTests
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
