module Main where

import TUI.UI (tui)

-- import Data.Aeson
-- import Data.ByteString.Lazy qualified as B
-- import RaffleizeDApp.CustomTypes.RaffleTypes (RaffleConfig)

-- import GeniusYield.Types
-- import RaffleizeDApp.TxBuilding.Context
-- import RaffleizeDApp.TxBuilding.Transactions
-- import RaffleizeDApp.TxBuilding.Validators (raffleizeValidatorGY)
-- import System.Environment

main :: IO () -- TODO (develop cli)
main = tui

-- args <- getArgs
-- case args of
--   [skey_file, _] -> do
--     skey <- readPaymentSigningKey skey_file
--     void $ runContextWithCfgProviders "deploy validator" $ deployReferenceScriptTransaction skey (validatorToScript raffleizeValidatorGY)
--   _ -> error "cabal run cli <skey_file> <raffle_config_file>"

--   -- MINT TEST TOKENS
--   void $ runContextWithCfgProviders "mint test tokens" $ mintTestTokensTransaction skey
--   print ("TEST TOKENS MINTED" :: String)
-- [skey_file, raffle_config_file] -> do
--   -- CREATE RAFFLE
--   skey <- readPaymentSigningKey skey_file
--   bs <- B.readFile raffle_config_file
--   let raffle_config = fromJust $ decode @RaffleConfig bs
--   print raffle_config
--   void $ runContextWithCfgProviders "create raffle" $ createRaffleTransaction skey raffle_config
--   print ("RAFFLE CREATED" :: String)