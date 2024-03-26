{-# LANGUAGE OverloadedStrings #-}

module TUI.UI where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Graphics.Vty.Input.Events

import GeniusYield.GYConfig (GYCoreConfig)
import GeniusYield.Types (GYPaymentSigningKey)

import Brick.Widgets.Core

import Brick.Util
import Brick.Widgets.Border.Style
import Graphics.Vty

import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad.IO.Class
import Data.List.Extra (intersperse)

import RaffleizeDApp.Constants (atlasCoreConfig, operationSkeyFilePath, raffleizeLogoPath, raffleizeValidatorsConfig)
import RaffleizeDApp.TxBuilding.Interactions

import System.IO.Extra (readFile)
import TUI.Actions

-- Types

data RaffleizeUI = RaffleizeUI
  { atlasConfig :: Maybe GYCoreConfig
  , validatorsConfig :: Maybe RaffleizeTxBuildingContext
  , adminSkey :: Maybe GYPaymentSigningKey
  , logo :: String
  , message :: String
  , refreshed :: Bool
  }
  deriving (Show)

data RaffleizeEvent = RaffleizeEvent

{- | Named resources

Not currently used, but will be easier to refactor
if we call this "Name" now.
-}
type Name = ()

-- App definition

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

-- Building the initial state

buildInitialState :: IO RaffleizeUI
buildInitialState = do
  logo <- readFile raffleizeLogoPath
  skey <- readPaymentKeyFile operationSkeyFilePath
  atlasConfig <- decodeConfigFile @GYCoreConfig atlasCoreConfig
  validatorsConfig <- decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  pure (RaffleizeUI atlasConfig validatorsConfig skey logo mempty False)

updateFromConfigFiles :: RaffleizeUI -> IO RaffleizeUI
updateFromConfigFiles s = do
  skey <- readPaymentKeyFile operationSkeyFilePath
  atlasConfig <- decodeConfigFile @GYCoreConfig atlasCoreConfig
  validatorsConfig <- decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  return $ s {adminSkey = skey, atlasConfig = atlasConfig, validatorsConfig = validatorsConfig}

-- Handling events

handleEvent :: RaffleizeUI -> BrickEvent Name RaffleizeEvent -> EventM Name (Next RaffleizeUI)
handleEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'b') [] -> continue s {message = ""}
    EvKey (KChar 'r') [] -> continue s {refreshed = not (refreshed s)}
    EvKey (KChar 'q') [] -> halt s
    EvKey (KChar 'g') [] -> do
      liftIO $ generateNewAdminSkey operationSkeyFilePath
      s' <- liftIO $ updateFromConfigFiles s
      continue s'
    EvKey (KChar 'd') [] -> do
      liftIO deployValidators
      s' <- liftIO $ updateFromConfigFiles s
      continue s' {message = "DONE"}
    _ -> continue s
  _ -> continue s

-- Drawing

drawUI :: RaffleizeUI -> [Widget Name]
drawUI s =
  joinBorders . withBorderStyle unicode . borderWithLabel (str "RAFFLEIZE - C.A.R.D.A.N.A")
    <$> [ if null (message s) then emptyWidget else center (str (logo s)) <=> str "[B] - Back"
        , if refreshed s then emptyWidget else mainMenu s
        , mainMenu s
        ]

mainMenu :: RaffleizeUI -> Widget n
mainMenu s =
  vBox
    [ center $ withAttr "highlight" $ str (logo s)
    , hBorder
    , hBox $
        padAll 1
          <$> [ str "Altas Config: " <+> showSymbol (isJust . atlasConfig $ s)
              , str "Validators Config: " <+> showSymbol (isJust . validatorsConfig $ s)
              , str "Admin Secret Key: " <+> showSymbol (isJust . adminSkey $ s)
              ]
    , hBorder
    , vBox $
        intersperse
          hBorder
          ( str
              <$> filter
                (not . null)
                [ "[D] - Deploy Raffleize Validators"
                , if isNothing . adminSkey $ s then "[G] - Generate new admin skey" else mempty
                , "[R] - Refresh screen"
                , "[Q] - Quit"
                ]
          )
    ]

theMap :: AttrMap
theMap =
  attrMap
    (white `on` black)
    [ ("highlight", fg magenta)
    , ("warning", fg red)
    , ("good", fg green)
    ]

showSymbol :: Bool -> Widget n
showSymbol True = withAttr "good" $ str "✔"
showSymbol False = withAttr "warning" $ str "X"