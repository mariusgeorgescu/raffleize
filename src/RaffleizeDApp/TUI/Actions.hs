module RaffleizeDApp.TUI.Actions where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.String
import GeniusYield.Types
import PlutusLedgerApi.V1 qualified
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.TUI.Utils
import RaffleizeDApp.TxBuilding.Context

import Data.Text qualified
import PlutusLedgerApi.V1.Value (AssetClass)
import RaffleizeDApp.CustomTypes.TicketTypes (TicketInfo)
import RaffleizeDApp.TxBuilding.Interactions (RaffleizeTxBuildingContext)
import RaffleizeDApp.TxBuilding.Transactions

deployValidators :: IO ()
deployValidators = do
  skey <- fromMaybe (error "Skey File Missing") <$> readPaymentKeyFile operationSkeyFilePath
  let msg = "Deploying validators"
  liftIO $ print msg
  validators <- runContextWithCfgProviders (fromString msg) $ deployRaffleizeValidators skey
  liftIO $ B.writeFile raffleizeValidatorsConfig (encode . toJSON $ validators)

getWalletAddress :: GYPaymentSigningKey -> IO GYAddress
getWalletAddress skey = do
  let msg = "Getting address"
  liftIO $ print msg
  runContextWithCfgProviders (fromString msg) $ queryGetAddressFromSkey skey

getAddrUTxOs :: GYAddress -> IO GYUTxOs
getAddrUTxOs addr = do
  let msg = "Getting UTxOs from " <> Data.Text.unpack (addressToText addr)
  liftIO $ print msg
  runContextWithCfgProviders (fromString msg) $ queryGetUTxOs addr

getAdaBalance :: GYUTxOs -> Ada
getAdaBalance = fromValue . getValueBalance

getValueBalance :: GYUTxOs -> PlutusLedgerApi.V1.Value
getValueBalance = valueToPlutus . foldMapUTxOs utxoValue

getAddressAndValue :: GYPaymentSigningKey -> IO (GYAddress, PlutusLedgerApi.V1.Value)
getAddressAndValue skey = do
  addr <- getWalletAddress skey
  utxos <- getAddrUTxOs addr
  return (addr, getValueBalance utxos)

mintTestTokens :: GYPaymentSigningKey -> String -> Integer -> IO Text
mintTestTokens skey tn amount = do
  let msg = "Minting test tokens"
  liftIO $ print msg
  r <- runContextWithCfgProviders (fromString msg) $ mintTestTokensTransaction skey tn amount
  return $ showTxOutRef r

createRaffle :: GYPaymentSigningKey -> RaffleConfig -> RaffleizeTxBuildingContext -> IO Text
createRaffle skey raffle validatorsTxOutRefs = do
  print emptyString
  print raffle
  let msg = "Create raffle"
  liftIO $ print msg
  r <- runContextWithCfgProviders (fromString msg) $ createRaffleTransaction skey raffle validatorsTxOutRefs
  return $ showTxOutRef r

buyTicket :: GYPaymentSigningKey -> String -> AssetClass -> Maybe GYAddress -> RaffleizeTxBuildingContext -> IO Text
buyTicket skey secretString raffleId mRecipient validatorsTxOutRefs = do
  print emptyString
  print secretString
  let msg = "Buy ticket"
  liftIO $ print msg
  r <- runContextWithCfgProviders (fromString msg) $ buyTicketTransaction skey secretString raffleId mRecipient validatorsTxOutRefs
  return $ showTxOutRef r

getActiveRaffles :: IO [RaffleInfo]
getActiveRaffles = do
  let msg = "Getting active raffles"
  liftIO $ print msg
  runContextWithCfgProviders (fromString msg) queryRafflesInfos

getMyRaffles :: GYAddress -> IO [RaffleInfo]
getMyRaffles addr = do
  let msg = "Getting my raffles"
  liftIO $ print msg
  runContextWithCfgProviders (fromString msg) (queryMyRaffles addr)

getMyTickets :: GYAddress -> IO [TicketInfo]
getMyTickets addr = do
  let msg = "Getting my tickets"
  liftIO $ print msg
  runContextWithCfgProviders (fromString msg) (queryMyTickets addr)