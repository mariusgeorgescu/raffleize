{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy qualified as B

import RaffleizeDApp.CustomTypes.RaffleTypes (RaffleConfig)
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Transactions

jsonFile :: FilePath
jsonFile = "raffleconfig.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO () -- TODO (develop cli)
main = do
  bs <- getJSON
  let raffle_config = fromJust $ decode @RaffleConfig bs
  print raffle_config
  skey_file <- parseArgs
  void $ runContextWithCfgProviders "query address" $ queryGetAddressFromSkeyFile skey_file

-- skey <- readPaymentSigningKey skey_file

-- -- withCfgProviders coreConfig ("mint" :: GYLogNamespace) $ \providers ->
-- --   do
-- --     let ctx = Ctx coreConfig providers
-- --     mintTestTokensTransaction ctx skey_file
-- withCfgProviders coreConfig ("Create" :: GYLogNamespace) $ \providers ->
--   do
--     let ctx = Ctx coreConfig providers
--     createRaffleTransaction ctx skey_file raffle_config

-- print addr
-- return ()
