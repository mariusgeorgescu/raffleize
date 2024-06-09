module RaffleizeDApp.TUI.Actions where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson hiding (Value)
import Data.ByteString.Lazy qualified as B
import Data.String
import Data.Text qualified
import GeniusYield.GYConfig
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import PlutusTx.Builtins (blake2b_256)
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.Tests.UnitTests
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Interactions
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Transactions
import RaffleizeDApp.TxBuilding.Utils

addressFromSkey :: ProviderCtx -> GYPaymentSigningKey -> GYAddress
addressFromSkey pCtx skey =
  let nid = (cfgNetworkId . ctxCoreCfg) pCtx
   in addressFromPaymentSigningKey nid skey

getAddrUTxOs :: ProviderCtx -> GYAddress -> IO GYUTxOs
getAddrUTxOs pCtx addr = do
  let msg = "Getting UTxOs of: \n\t" <> Data.Text.unpack (addressToText addr)
  putStrLn $ yellowColorString msg
  runQuery pCtx $ lookupUTxOsAtfAddress addr

getAddressAndValue :: ProviderCtx -> GYPaymentSigningKey -> IO (GYAddress, Value)
getAddressAndValue pCtx skey = do
  let addr = addressFromSkey pCtx skey
  utxos <- getAddrUTxOs pCtx addr
  return (addr, getValueBalance utxos)

getActiveRaffles :: ProviderCtx -> IO [RaffleInfo]
getActiveRaffles pCtx = do
  putStrLn $ yellowColorString ("Getting active raffles info .." :: String)
  runQuery pCtx lookupActiveRaffles

getMyTickets :: ProviderCtx -> GYAddress -> IO [TicketInfo]
getMyTickets pCtx addr = do
  let msg = "Getting tickets of: \n\t" <> Data.Text.unpack (addressToText addr)
  putStrLn $ yellowColorString msg
  runQuery pCtx (lookupTicketsOfAddress addr)

getMyRaffles :: ProviderCtx -> GYAddress -> IO [RaffleInfo]
getMyRaffles pCtx addr = do
  let msg = "Getting raffles of: \n\t " <> Data.Text.unpack (addressToText addr)
  putStrLn $ yellowColorString msg
  runQuery pCtx (lookupRafflesOfAddress addr)

----------------
----------------
----------------
----------------
deployValidators :: ProviderCtx -> GYPaymentSigningKey -> IO ()
deployValidators pCtx skey = do
  putStrLn $ yellowColorString ("Deploying validators ..." :: String)
  let nid = (cfgNetworkId . ctxCoreCfg) pCtx
  let my_addr = addressFromPaymentSigningKey nid skey
  let userAddresses = UserAddresses [my_addr] my_addr Nothing
  rTxBody <- runReaderT (deployRaffleizeValidatortTxBody userAddresses) pCtx
  let rSigned = signGYTxBody rTxBody [skey]
  rTxOutRef <- runReaderT (submitTxAndWaitForConfirmation rSigned) pCtx
  putStrLn $ yellowColorString ("1/2 deployed ..." :: String)
  tTxBody <- runReaderT (deployTicketValidatortTxBody userAddresses) pCtx
  let tSigned = signGYTxBody tTxBody [skey]
  tTxOutRef <- runReaderT (submitTxAndWaitForConfirmation tSigned) pCtx
  putStrLn $ yellowColorString ("2/2 deployed ..." :: String)
  let validators = RaffleizeTxBuildingContext {raffleValidatorRef = rTxOutRef, ticketValidatorRef = tTxOutRef}
  B.writeFile raffleizeValidatorsConfig (encode . toJSON $ validators)
  putStrLn $ greenColorString ("exported to " <> raffleizeValidatorsConfig)

mintTestTokens :: ProviderCtx -> GYPaymentSigningKey -> GYAddress -> String -> Integer -> IO Text
mintTestTokens pCtx skey addr tn amount = do
  putStrLn $ yellowColorString "Minting test tokens... "
  putStrLn $ blueColorString $ "with token name: " <> show tn
  putStrLn $ blueColorString $ "amount: " <> show amount
  let userAddrs = UserAddresses [addr] addr Nothing
  mintTxBody <- runReaderT (mintTestTokensTxBody userAddrs tn amount) pCtx
  let mintTxSigned = signGYTxBody mintTxBody [skey]
  txOutRef <- runReaderT (submitTxAndWaitForConfirmation mintTxSigned) pCtx
  return $ showTxOutRef txOutRef

raffleizeTransaction :: RaffleizeOffchainContext -> GYPaymentSigningKey -> RaffleizeAction -> Maybe AssetClass -> Maybe GYAddress -> IO Text
raffleizeTransaction raffleizeContext@RaffleizeOffchainContext {..} skey raffleizeActon interactionContextNFT optionalRecipient = do
  let my_addr = addressFromSkey providerCtx skey
  let userAddrs = UserAddresses [my_addr] my_addr Nothing
  let raffleizeInteraction = RaffleizeInteraction interactionContextNFT raffleizeActon userAddrs optionalRecipient
  putStrLn (yellowColorString "Building transaction...")
  raffleizeTxBody <- runReaderT (interactionToTxBody raffleizeInteraction) raffleizeContext
  let raffleizeTxSigned = signGYTxBody raffleizeTxBody [skey]
  putStrLn $ greenColorString ("Buit and signed by: \n\t" <> Data.Text.unpack (addressToText my_addr))
  txOutRef <- runReaderT (submitTxAndWaitForConfirmation raffleizeTxSigned) providerCtx
  return $ showTxOutRef txOutRef

createRaffle :: RaffleizeOffchainContext -> GYPaymentSigningKey -> RaffleConfig -> Maybe GYAddress -> IO Text
createRaffle roc skey config mRecipient = do
  putStrLn $ yellowColorString ("Creating a new raffle" :: String)
  putStrLn $ blueColorString (show config)
  raffleizeTransaction roc skey (User (CreateRaffle config)) Nothing mRecipient

buyTicket :: RaffleizeOffchainContext -> GYPaymentSigningKey -> String -> AssetClass -> Maybe GYAddress -> IO Text
buyTicket roc skey secretString raffleId mRecipient = do
  let secretHash = blake2b_256 $ fromString @BuiltinByteString secretString
  putStrLn $ yellowColorString $ "Buying ticket to raffle: \n\t " <> show raffleId
  putStrLn $ blueColorString $ "with secret: " <> secretString
  putStrLn $ blueColorString $ "onchain secret hash: " <> (show . toJSON $ secretHash)
  raffleizeTransaction roc skey (User (BuyTicket secretHash)) (Just raffleId) mRecipient

----------------

---------
