{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy qualified as B

import GeniusYield.Types
import RaffleizeDApp.CustomTypes.RaffleTypes (RaffleConfig)
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Transactions
import System.Environment

main :: IO () -- TODO (develop cli)
main = do
  args <- getArgs
  case args of
    [skey_file] -> do
      skey <- readPaymentSigningKey skey_file

      -- MINT TEST TOKENS
      void $ runContextWithCfgProviders "mint test tokens" $ mintTestTokensTransaction skey
      print ("TEST TOKENS MINTED" :: String)
    [skey_file, raffle_config_file] -> do
      -- CREATE RAFFLE
      skey <- readPaymentSigningKey skey_file
      bs <- B.readFile raffle_config_file
      let raffle_config = fromJust $ decode @RaffleConfig bs
      print raffle_config
      void $ runContextWithCfgProviders "create raffle" $ createRaffleTransaction skey raffle_config
      print ("RAFFLE CREATED" :: String)
    _ -> error "cabal run cli <skey_file> <raffle_config_file>"