module RaffleizeDApp.TUI.Actions where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson hiding (Value)
import Data.ByteString.Lazy qualified as B
import Data.Text qualified
import GeniusYield.GYConfig
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import PlutusTx.Show qualified (show)
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.TransferTypes
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Lookups
import RaffleizeDApp.TxBuilding.Transactions
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.Utils

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

mintTestTokens :: ProviderCtx -> GYPaymentSigningKey -> String -> Integer -> IO Text
mintTestTokens pCtx skey tn amount = do
  putStrLn $ yellowColorString "Minting test tokens... "
  putStrLn $ blueColorString $ "with token name: " <> show tn
  putStrLn $ blueColorString $ "amount: " <> show amount
  let my_addr = addressFromSkey pCtx skey
  let userAddrs = UserAddresses [my_addr] my_addr Nothing
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
  putStrLn $ greenColorString ("Built and signed by: \n\t" <> Data.Text.unpack (addressToText my_addr))
  txOutRef <- runReaderT (submitTxAndWaitForConfirmation raffleizeTxSigned) providerCtx
  return $ showTxOutRef txOutRef

raffleizeActionToIntro :: Maybe AssetClass -> RaffleizeAction -> IO ()
raffleizeActionToIntro ma ra =
  let inContextOf s = case ma of
        (Just contextNFT) -> printf "\n Transaction in context of %s:\n\t" s <> show contextNFT
        Nothing -> ""
   in do
        case ra of
          User a -> case a of
            (CreateRaffle rconfig) -> do
              putStrLn $ yellowColorString "Creating a new raffle..."
              putStrLn $ blueColorString (show rconfig)
            (BuyTicket secretHashBS) -> do
              putStrLn $ yellowColorString "Buying ticket to raffle... \n\t "
              putStrLn $ blueColorString $ "onchain secret hash: " <> Data.Text.unpack (fromBuiltin @BuiltinString @Text $ PlutusTx.Show.show secretHashBS)
          RaffleOwner roa -> do
            putStrLn $ inContextOf ("raffle" :: String)
            case roa of
              (Update rconfig) -> do
                putStrLn $ yellowColorString "Updating raffle configuration... \n\t "
                putStrLn "New raffle configuration"
                putStrLn $ blueColorString (show rconfig)
              Cancel -> do
                putStrLn $ yellowColorString "Cancelling raffle... \n\t "
              RecoverStake -> do
                putStrLn $ yellowColorString "Recovering stake from failed raffle... \n\t "
              RecoverStakeAndAmount -> do
                putStrLn $ yellowColorString "Recovering stake from unrevealed raffle... \n\t "
              CollectAmount -> do
                putStrLn $ yellowColorString "Claiming collected amount from successfull raffle... \n\t "
              GetCollateralOfExpiredTicket -> do
                putStrLn $ yellowColorString "Getting the ticket collateral refund for losing ticket of raffle... \n\t "
          TicketOwner toa -> do
            putStrLn $ inContextOf ("ticket" :: String)
            case toa of
              (RevealTicketSecret secretBS) -> do
                putStrLn $ yellowColorString "Reavealing the ticket secret... \n\t "
                putStrLn $ blueColorString $ "onchain revealed secret: " <> Data.Text.unpack (fromBuiltin @BuiltinString @Text $ PlutusTx.Show.show secretBS)
              CollectStake -> do
                putStrLn $ yellowColorString "Collecting stake with winning ticket: \n\t "
              RefundTicket -> do
                putStrLn $ yellowColorString "Getting full refund for ticket of underfunded raffle: \n\t "
              RefundTicketExtra -> do
                putStrLn $ yellowColorString "Getting extra refund for ticket of unrevealed raffle: \n\t "
              RefundCollateralLosing -> do
                putStrLn $ yellowColorString "Getting the ticket collateral refund for losing ticket of raffle: \n\t "
          Admin CloseRaffle -> undefined -- TODO

----------------

---------
