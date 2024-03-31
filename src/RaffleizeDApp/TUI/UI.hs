{-# LANGUAGE OverloadedStrings #-}

module RaffleizeDApp.TUI.UI where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Graphics.Vty.Input.Events

import GeniusYield.GYConfig (GYCoreConfig (cfgCoreProvider, cfgNetworkId), GYCoreProviderInfo (..))
import GeniusYield.Types (Ada, GYAddress, GYNetworkId (..), GYPaymentSigningKey, GYTxOutRef, addressToText, showTxOutRef)

import Brick.Widgets.Core

import Brick.Util
import Brick.Widgets.Border.Style
import Graphics.Vty

import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad.IO.Class

import RaffleizeDApp.Constants (atlasCoreConfig, operationSkeyFilePath, raffleizeLogoPath, raffleizeValidatorsConfig)
import RaffleizeDApp.TxBuilding.Interactions

import Brick.Widgets.Table (renderTable, table)
import Data.Char
import Data.Text (unpack)
import RaffleizeDApp.TUI.Actions
import RaffleizeDApp.TUI.Utils
import System.Console.ANSI (clearScreen)
import System.IO.Extra (readFile)

------------------------------------------------------------------------------------------------

-- *   Types

------------------------------------------------------------------------------------------------

data RaffleizeUI = RaffleizeUI
  { atlasConfig :: Maybe GYCoreConfig
  , validatorsConfig :: Maybe RaffleizeTxBuildingContext
  , adminSkey :: Maybe GYPaymentSigningKey
  , adminAddress :: Maybe GYAddress
  , adminBalance :: Maybe Ada
  , logo :: String
  , message :: String
  }
  deriving (Show)

data RaffleizeEvent = RaffleizeEvent

{- | Named resources

Not currently used, but will be easier to refactor
if we call this "Name" now.
-}
type Name = ()

------------------------------------------------------------------------------------------------

-- *   App definitionF

------------------------------------------------------------------------------------------------

app :: App RaffleizeUI RaffleizeEvent Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return $ liftIO buildInitialState
    , appAttrMap = const theMap
    }

tui :: IO ()
tui = do
  initialState <- buildInitialState
  _endState <- defaultMain app initialState
  return ()

------------------------------------------------------------------------------------------------

-- *  Building the initial state

------------------------------------------------------------------------------------------------
buildInitialState :: IO RaffleizeUI
buildInitialState = do
  logo <- readFile raffleizeLogoPath
  skey <- readPaymentKeyFile operationSkeyFilePath
  atlasConfig <- decodeConfigFile @GYCoreConfig atlasCoreConfig
  validatorsConfig <- decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  pure (RaffleizeUI atlasConfig validatorsConfig skey Nothing Nothing logo mempty)

------------------------------------------------------------------------------------------------

-- *  Handling Events

------------------------------------------------------------------------------------------------

handleEvent :: RaffleizeUI -> BrickEvent Name RaffleizeEvent -> EventM Name (Next RaffleizeUI)
handleEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar c) [] | c `elem` ("qQ" :: [Char]) -> halt s
    EvKey (KChar c) [] | c `elem` ("lL" :: [Char]) && isJust (adminSkey s) -> do
      let skey = fromMaybe (error "No skey") $ adminSkey s
      (addr, val) <- liftIO $ getAddressAndValue skey
      continue s {adminAddress = Just addr, adminBalance = Just val}
    EvKey (KChar c) [] | c `elem` ("bB" :: [Char]) -> continue s {message = ""}
    EvKey (KChar c) [] | c `elem` ("rR" :: [Char]) -> continue s {message = "Refresh Screen"}
    EvKey (KChar c) [] | c `elem` ("gG" :: [Char]) -> do
      liftIO $ generateNewAdminSkey operationSkeyFilePath
      s' <- liftIO $ updateFromConfigFiles s
      continue s'
    EvKey (KChar c) [] | c `elem` ("dD" :: [Char]) && isJust (adminSkey s) -> do
      liftIO clearScreen
      liftIO $ print ("Deploying validators ...." :: String)
      liftIO $ print ("Building, signing and submiting transactions and waiting for confirmations.." :: String)
      liftIO deployValidators
      s' <- liftIO $ updateFromConfigFiles s
      continue s' {message = "VALIDATORS SUCCESFULLY DEPLOYED !\nTxOuts references are saved to " ++ show raffleizeValidatorsConfig}
    _ -> continue s
  _ -> continue s

updateFromConfigFiles :: RaffleizeUI -> IO RaffleizeUI
updateFromConfigFiles s = do
  skey <- readPaymentKeyFile operationSkeyFilePath
  atlasConfig <- decodeConfigFile @GYCoreConfig atlasCoreConfig
  validatorsConfig <- decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  return $ s {adminSkey = skey, atlasConfig = atlasConfig, validatorsConfig = validatorsConfig}

------------------------------------------------------------------------------------------------

-- *  Drawing

------------------------------------------------------------------------------------------------

theMap :: AttrMap
theMap =
  attrMap
    (white `on` black)
    [ ("highlight", fg magenta)
    , ("warning", fg red)
    , ("good", fg green)
    , ("action", fg yellow)
    ]

drawUI :: RaffleizeUI -> [Widget Name]
drawUI s =
  joinBorders . withBorderStyle unicode . borderWithLabel (str "RAFFLEIZE - C.A.R.D.A.N.A")
    <$> [ if null (message s) then emptyWidget else center (withAttr "highlight" $ str (message s)) <=> str "[B] - Back"
        , mainMenu s
        ]

------------------------------------------------------------------------------------------------

-- **  Widgets

------------------------------------------------------------------------------------------------

mainMenu :: RaffleizeUI -> Widget n
mainMenu s =
  vBox
    [ center $ withAttr "highlight" $ str (logo s)
    , hBorder
    , hCenter $ configFilesWidget s
    , hBorder
    , hCenter $ availableActionsWidget s
    ]

configFilesWidget :: RaffleizeUI -> Widget n
configFilesWidget s =
  borderWithLabel (str "CONFIGURATION") $
    hBox $
      padAll 1
        <$> [ providersWidget (atlasConfig s)
            , validatorsWidget (validatorsConfig s)
            , adminWidget (adminSkey s) (adminAddress s) (adminBalance s)
            ]

symbolWidget :: Bool -> Widget n
symbolWidget True = withAttr "good" $ str "✔"
symbolWidget False = withAttr "warning" $ str "X"

validatorsWidget :: Maybe RaffleizeTxBuildingContext -> Widget n
validatorsWidget mv =
  borderWithLabel (str "Validators TxOuts\n with Reference Scripts ") $
    renderTable $
      table $
        [str "Loaded: ", symbolWidget (isJust mv)] : case mv of
          Nothing -> []
          Just (RaffleizeTxBuildingContext {..}) ->
            [ [str "Raffle Validator", txOutRefWidget raffleValidatorRef]
            , [str "Ticket Validator", txOutRefWidget ticketValidatorRef]
            ]

txOutRefWidget :: GYTxOutRef -> Widget n
txOutRefWidget t = withAttr "good" $ str (unpack $ showTxOutRef t)

providersWidget :: Maybe GYCoreConfig -> Widget n
providersWidget mp =
  borderWithLabel (str "Blockchain Provider") $
    renderTable $
      table $
        [ str "Loaded: "
        , symbolWidget (isJust mp)
        ]
          : case mp of
            Nothing -> []
            Just cfg ->
              [ [str "Provider: ", printProvider cfg]
              , [str "Network: ", printNetwork cfg]
              ]

printProvider :: GYCoreConfig -> Widget n
printProvider cfg = withAttr "good" . str $ case cfgCoreProvider cfg of
  (GYNodeKupo {}) -> "KUPO"
  (GYMaestro {}) -> "MAESTRO"
  (GYBlockfrost {}) -> "BLOCKFROST"

printNetwork :: GYCoreConfig -> Widget n
printNetwork cfg = withAttr "good" . str $ case cfgNetworkId cfg of
  GYMainnet -> "MAINNET"
  GYTestnetPreprod -> "PREPROD"
  GYTestnetPreview -> "PREVIEW"
  GYTestnetLegacy -> "TESTNET-LEGACY"
  GYPrivnet -> "PRIVNET"

adaBalanceWidget :: Ada -> Widget n
adaBalanceWidget val = withAttr "good" $ str (show val)

addressWidget :: GYAddress -> Widget n
addressWidget addr = withAttr "good" $ str (show . unpack $ addressToText addr)

adminWidget :: Maybe a -> Maybe GYAddress -> Maybe Ada -> Widget n
adminWidget ma maddr mbal =
  borderWithLabel (str "Admin") $
    renderTable $
      table $
        [str "Loaded: ", symbolWidget (isJust ma)] : case ma of
          Nothing -> []
          Just _ -> case (maddr, mbal) of
            (Just addr, Just bal) ->
              [ [str "Address: ", addressWidget addr]
              , [str "Balance: ", adaBalanceWidget bal]
              ]
            _ -> []

availableActionsWidget :: RaffleizeUI -> Widget n
availableActionsWidget s =
  withAttr "action" . borderWithLabel (str "AVAILABLE ACTIONS") $
    vBox
      ( str
          <$> filter
            (not . null)
            ( ( if isNothing . adminSkey $ s
                  then ["[G] - Generate new admin skey"]
                  else ["[D] - Deploy Raffleize Validators", "[L] - Query current admin balance"]
              )
                ++ [ "[R] - Refresh screen"
                   , "[Q] - Quit"
                   ]
            )
      )
