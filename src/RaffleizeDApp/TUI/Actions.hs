module RaffleizeDApp.TUI.Actions where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.String
import Data.Text qualified
import GeniusYield.Types
import PlutusLedgerApi.V1 qualified
import PlutusLedgerApi.V1.Value
import PlutusTx.Builtins (blake2b_256)
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.TUI.Utils
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions
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

raffleizeTransaction :: GYPaymentSigningKey -> RaffleizeAction -> Maybe AssetClass -> Maybe GYAddress -> RaffleizeTxBuildingContext -> IO Text
raffleizeTransaction skey raffleizeActon interactionContextNFT optionalRecipient validatorsTxOutRefs = do
  my_addr <- getWalletAddress skey
  let userAddrs = UserAddresses [my_addr] my_addr Nothing
  let raffleizeInteraction = RaffleizeInteraction interactionContextNFT raffleizeActon userAddrs optionalRecipient
  raffleizeTxBody <- runContextWithCfgProviders (fromString (show raffleizeActon)) $ buildRaffleizeTxBody raffleizeInteraction validatorsTxOutRefs
  let raffleizeTxSigned = signGYTxBody raffleizeTxBody [skey]
  txOutRef <- runContextWithCfgProviders (fromString (show raffleizeActon)) $ submitTxAndWaitForConfirmation raffleizeTxSigned
  return $ showTxOutRef txOutRef

createRaffle :: GYPaymentSigningKey -> RaffleConfig -> Maybe GYAddress -> RaffleizeTxBuildingContext -> IO Text
createRaffle skey config mRecipient validatorsTxOutRefs = do
  liftIO $ print ("Creating a new raffle" :: String)
  raffleizeTransaction skey (User (CreateRaffle config)) Nothing mRecipient validatorsTxOutRefs

buyTicket :: GYPaymentSigningKey -> String -> AssetClass -> Maybe GYAddress -> RaffleizeTxBuildingContext -> IO Text
buyTicket skey secretString raffleId mRecipient validatorsTxOutRefs = do
  liftIO $ print $ "Buying ticket to raffle: " <> show raffleId <> " with secret: " <> secretString
  let secretHash = blake2b_256 $ fromString @BuiltinByteString secretString
  raffleizeTransaction skey (User (BuyTicket secretHash)) (Just raffleId) mRecipient validatorsTxOutRefs

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