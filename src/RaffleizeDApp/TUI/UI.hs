{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module RaffleizeDApp.TUI.UI where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Graphics.Vty.Input.Events

import GeniusYield.GYConfig (GYCoreConfig (cfgCoreProvider, cfgNetworkId), GYCoreProviderInfo (..))
import GeniusYield.Types (GYAddress, GYNetworkId (..), GYPaymentSigningKey, GYTxOutRef, addressToText, fromValue, showTxOutRef)

import Brick.Widgets.Core

import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Lens
import Control.Monad.IO.Class
import Graphics.Vty

import RaffleizeDApp.Constants
import RaffleizeDApp.TxBuilding.Interactions

import Brick.Forms
import Brick.Widgets.List (Splittable (splitAt), list, renderList)
import Brick.Widgets.Table (renderTable, table)
import Control.Monad.State (modify)
import Control.Monad.State.Class (gets)
import Data.Char
import Data.Csv (Name)
import Data.List (intercalate)
import Data.String (IsString)
import Data.String qualified
import Data.Text (unpack)
import PlutusLedgerApi.V1.Value
import RaffleizeDApp.TUI.Actions
import RaffleizeDApp.TUI.Utils
import RaffleizeDApp.TxBuilding.Validators (exportMintingPolicy, exportRaffleScript, exportTicketScript)
import System.Console.ANSI (clearScreen)
import System.IO.Extra (readFile)


-----

-------

data NameResources
  = ValueItemsList
  | ValueItemsViewPort
  | TokenNameField
  | MintAmount
  | Other
  deriving (Eq, Ord, Show, Generic)

instance IsString NameResources where
  fromString :: String -> NameResources
  fromString "TokenNameField" = TokenNameField
  fromString "MintAmount" = MintAmount
  fromString "ValueItemsViewPort" = ValueItemsViewPort
  fromString "ValueItemsList" = ValueItemsList
  fromString _ = Other

data MintTokenForm = MintTokenForm
  { _tokenNameField :: Text
  , _mintAmount :: Int
  }
  deriving (Show)

makeLenses ''MintTokenForm

mkForm :: MintTokenForm -> Form MintTokenForm e NameResources
mkForm =
  newForm [(str "Name: " <+>) @@= editTextField tokenNameField TokenNameField (Just 1)
          , (str "Amount: " <+>) @@= editShowableFieldWithValidate mintAmount  MintAmount (==100) ]

------------------------------------------------------------------------------------------------

-- *   Types

------------------------------------------------------------------------------------------------

data RaffleizeUI = RaffleizeUI
  { atlasConfig :: Maybe GYCoreConfig
  , validatorsConfig :: Maybe RaffleizeTxBuildingContext
  , adminSkey :: Maybe GYPaymentSigningKey
  , adminAddress :: Maybe GYAddress
  , adminBalance :: Maybe Value
  , logo :: String
  , message :: String
  , mintTokenForm :: Form MintTokenForm RaffleizeEvent NameResources
  , unlockKeys :: Bool
  }

data RaffleizeEvent = RaffleizeEvent

------------------------------------------------------------------------------------------------

-- *   App definition

------------------------------------------------------------------------------------------------

app :: App RaffleizeUI RaffleizeEvent NameResources
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
  pure (RaffleizeUI atlasConfig validatorsConfig skey Nothing Nothing logo mempty (mkForm (MintTokenForm mempty 0)) False)

------------------------------------------------------------------------------------------------

-- *  Handling Events

------------------------------------------------------------------------------------------------

handleEvent :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources (Next RaffleizeUI)
handleEvent s e =
  case e of
    VtyEvent vtye -> case vtye of
      EvKey KEsc [] -> continue s {message = "", unlockKeys = False} -- handleEvent (s {message = "", unlockKeys = False}) (VtyEvent (EvKey (KChar 'l') [])) -- reload balance
      EvKey key _modifiers ->
        if unlockKeys s
          then do
            f <- handleFormEvent e (mintTokenForm s)
            continue s {mintTokenForm = f}
          else case key of
            KDown -> do
              let vscroll = viewportScroll ValueItemsViewPort
              vScrollBy vscroll 1
              continue s
            KUp -> do
              let vscroll = viewportScroll ValueItemsViewPort
              vScrollBy vscroll (-1)
              continue s
            (KChar c) -> case c of
              'p' -> continue s {unlockKeys = True}
              'q' -> halt s
              'r' -> continue s {message = "Refresh Screen"}
              'g' -> do
                liftIO $ generateNewAdminSkey operationSkeyFilePath
                s' <- liftIO $ updateFromConfigFiles s
                continue s'
              'e' -> do
                liftIO $ sequence_ [exportRaffleScript, exportTicketScript, exportMintingPolicy]
                continue s {message = "VALIDATORS SUCCESFULLY EXPORTED !\n" ++ intercalate "\n" [raffleizeValidatorFile, ticketValidatorFile, mintingPolicyFile]}
              _ ->
                if isNothing (adminSkey s)
                  then continue s
                  else case c of
                    'l' -> do
                      let skey = fromMaybe (error "No skey") $ adminSkey s
                      (addr, val) <- liftIO $ getAddressAndValue skey
                      continue s {adminAddress = Just addr, adminBalance = Just val}
                    'd' -> do
                      liftIO clearScreen
                      liftIO deployValidators
                      s' <- liftIO $ updateFromConfigFiles s
                      continue s' {message = "VALIDATORS SUCCESFULLY DEPLOYED !\nTxOuts references are saved to " ++ show raffleizeValidatorsConfig}
                    't' -> do
                      liftIO clearScreen
                      txOutRef <- liftIO $ mintTestTokens (fromJust (adminSkey s))
                      continue s {message = "TEST TOKENS SUCCESFULLY MINTED !\n" <> txOutRef}
                    'c' -> do
                      liftIO clearScreen
                      txOutRef <- liftIO $ createRaffle (fromJust (adminSkey s))
                      continue s {message = "RAFFLE SUCCESFULLY CREATED !\n" <> txOutRef}
                    _ -> continue s
            _ -> continue s
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

drawUI :: RaffleizeUI -> [Widget NameResources]
drawUI s =
  joinBorders . withBorderStyle unicode . borderWithLabel (str "RAFFLEIZE - C.A.R.D.A.N.A")
    <$> [ if not (unlockKeys s) then emptyWidget else center (border $ renderForm $ mintTokenForm s) <=> str "[ESC] - Close"
        , if null (message s) then emptyWidget else center (withAttr "highlight" $ str (message s)) <=> str "[ESC] - Close"
        , mainMenu s
        ]

------------------------------------------------------------------------------------------------

-- **  Widgets

------------------------------------------------------------------------------------------------

mainMenu :: RaffleizeUI -> Widget NameResources
mainMenu s =
  vBox
    [ hCenter $ withAttr "highlight" $ str (logo s)
    , hBorder
    , center $ bodyWidget s
    ]

bodyWidget :: RaffleizeUI -> Widget NameResources
bodyWidget s =
  vBox $
    padLeftRight 1
      <$> [ leftBodyWidget s
          , hBorder
          , assetsWidget (cfgNetworkId <$> atlasConfig s) (adminSkey s) (adminAddress s) (adminBalance s)
          ]

leftBodyWidget :: (Ord n, Show n, Data.String.IsString n) => RaffleizeUI -> Widget n
leftBodyWidget s =
  hCenter $
    hBox $
      padLeftRight 1
        <$> [ availableActionsWidget s
            , providersWidget (atlasConfig s)
            , validatorsWidget (cfgNetworkId <$> atlasConfig s) (validatorsConfig s)
            , walletFlagWidget (adminSkey s)
            ]

symbolWidget :: Bool -> Widget n
symbolWidget v = if v then withAttr "good" $ str "✔" else withAttr "warning" $ str "X"

validatorsWidget :: Maybe GYNetworkId -> Maybe RaffleizeTxBuildingContext -> Widget n
validatorsWidget mnid mv =
  borderWithLabel (str "VALIDATORS") $
    renderTable $
      table $
        [str "Deployed: ", symbolWidget (isJust mv)] : case (mnid, mv) of
          (Just nid, Just (RaffleizeTxBuildingContext {..})) ->
            [ [str "Raffle Validator", txOutRefWidget nid raffleValidatorRef]
            , [str "Ticket Validator", txOutRefWidget nid ticketValidatorRef]
            ]
          _ -> []

walletFlagWidget :: (Ord n, Show n, Data.String.IsString n) => Maybe a -> Widget n
walletFlagWidget mkey =
  borderWithLabel (str "WALLET") $
    renderTable $
      table
        [[str "Connected ", symbolWidget (isJust mkey)]]

txOutRefWidget :: GYNetworkId -> GYTxOutRef -> Widget n
txOutRefWidget nid t = withAttr "good" $ str (showLink nid "tx" $ unpack $ showTxOutRef t)

providersWidget :: Maybe GYCoreConfig -> Widget n
providersWidget mp =
  borderWithLabel (str "BLOCKCHAIN PROVIDER") $
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

showLink :: GYNetworkId -> String -> String -> String
showLink nid s content = case nid of
  GYMainnet -> cexplorerMainnet <> s <> "/" <> content <> " "
  GYTestnetPreprod -> cexplorerPreprod <> s <> "/" <> content <> " "
  GYTestnetPreview -> cexplorerPreview <> s <> "/" <> content <> " "
  GYTestnetLegacy -> content
  GYPrivnet -> content

adaBalanceWidget :: Value -> Widget n
adaBalanceWidget val = withAttr "good" $ str (show (fromValue val) ++ "\n")

instance Splittable [] where
  splitAt :: Int -> [a] -> ([a], [a])
  splitAt i = Prelude.splitAt (fromIntegral i)

assetsBalanceWidget :: Value -> Widget NameResources
assetsBalanceWidget val =
  let
    valueItemsList = Brick.Widgets.List.list ValueItemsList (flattenValue val) 5
   in
    borderWithLabel (str "ASSETS") $
      visible $
        withVScrollBarHandles $
          withVScrollBars OnRight $
            viewport ValueItemsViewPort Vertical $
              vLimit 300 $
                hLimit 110 $
                  renderList valueItemWidget False valueItemsList

assetClassItemWidget :: CurrencySymbol -> TokenName -> Widget n
assetClassItemWidget cs tn = str (show cs) <=> hBorder <=> str (toString tn)

valueItemWidget :: Bool -> (CurrencySymbol, TokenName, Integer) -> Widget n
valueItemWidget hasFocus (cs, tn, i) = vLimit 5 $ hLimit 100 (border (assetClassItemWidget cs tn <+> vBorder <+> center (str (show i))))

addressWidget :: GYNetworkId -> GYAddress -> Widget n
addressWidget nid addr = withAttr "good" $ str $ showLink nid "address" $ unpack $ addressToText addr

assetsWidget :: Maybe GYNetworkId -> Maybe a -> Maybe GYAddress -> Maybe Value -> Widget NameResources
assetsWidget (Just nid) (Just key) (Just addr) (Just val) =
  borderWithLabel (str "WALLET") $
    hBox
      [ assetsAdaWidget nid addr val
      , vBorder
      , assetsBalanceWidget val
      ]
assetsWidget _ _ _ _ = emptyWidget

assetsAdaWidget :: (Ord n, Show n, Data.String.IsString n) => GYNetworkId -> GYAddress -> Value -> Widget n
assetsAdaWidget nid addr val =
  renderTable $
    table
      [ [str "Address: ", addressWidget nid addr]
      , [str "Ada | ₳ |:", adaBalanceWidget val]
      ]

availableActionsWidget :: RaffleizeUI -> Widget n
availableActionsWidget s =
  withAttr "action" . borderWithLabel (str "AVAILABLE ACTIONS") $
    vBox
      ( str
          <$> filter
            (not . null)
            ( ( if isNothing . adminSkey $ s
                  then ["[G] - Generate new admin skey"]
                  else ["[D] - Deploy Raffleize Validators", "[L] - Get wallet balance", "[C] - Create raffle", "[T] - Mint some test tokens"]
              )
                ++ [ "[R] - Refresh screen"
                   , "[E] - Export validators"
                   , "[Q] - Quit"
                   ]
            )
      )
