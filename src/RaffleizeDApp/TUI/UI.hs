{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module RaffleizeDApp.TUI.UI where

import Brick.AttrMap
import Brick.Forms
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Table

import Control.Lens
import Control.Monad.IO.Class
import Data.List qualified
import Data.Text qualified

import Data.Aeson (ToJSON (toJSON))
import Data.Time
import Data.Time.Format.ISO8601
import Data.Vector qualified
import GHC.Real (Integral (div))
import GeniusYield.GYConfig
import GeniusYield.Types
import Graphics.Vty
import PlutusLedgerApi.V1.Time qualified
import PlutusLedgerApi.V1.Value
import PlutusPrelude (showText)
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.CustomTypes.Types
import RaffleizeDApp.TUI.Actions
import RaffleizeDApp.TUI.Utils

import RaffleizeDApp.Tests.UnitTests (yellowColorString)
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators
import System.Console.ANSI (clearScreen)
import System.IO.Extra (readFile)

assetClassItemWidget :: CurrencySymbol -> TokenName -> Widget n
assetClassItemWidget cs tn = str (show cs) <=> hBorder <=> str (toString tn)

valueItemWidget :: Bool -> Bool -> (CurrencySymbol, TokenName, Integer) -> Widget n
valueItemWidget selectable hasFocus (cs, tn, i) =
  (if hasFocus && selectable then withAttr "selected" else id) $
    vLimit 5 $
      hLimit 130 $
        border
          ( assetClassItemWidget cs tn
              <+> vBorder
              <+> center (str (show i))
          )

------------------------------------------------------------------------------------------------

-- *   Types

------------------------------------------------------------------------------------------------

data NameResources
  = CommitDdlField
  | RevealDdlField
  | MinNoTokensField
  | StakeValueListField
  | StakeAmountField
  | TicketPriceField
  | ActiveRafflesListField
  | MyRafflesListField
  | MyTicketsListField
  | ValueItemsList
  | ValueItemsViewPort
  | TokenNameField
  | MintAmountField
  | SecretField
  | SendTicketAddressField
  | SendRaffleAddressField
  | Other
  deriving (Eq, Ord, Show, Generic)

------------------------------------------------------------------------------------------------

-- **  Mint test tokens form

------------------------------------------------------------------------------------------------

data MintTokenFormState = MintTokenFormState
  { _tokenNameField :: Text
  , _mintAmount :: Int
  }
  deriving (Show)

makeLenses ''MintTokenFormState

mkMintTokenForm :: MintTokenFormState -> Form MintTokenFormState e NameResources
mkMintTokenForm =
  newForm
    [ (txt "Token name: " <+>) @@= editTextField tokenNameField TokenNameField (Just 1)
    , (txt "Minting amount: " <+>) @@= editShowableField mintAmount MintAmountField
    ]

------------------------------------------------------------------------------------------------

-- **  Create new raffle form

------------------------------------------------------------------------------------------------

data CreateRaffleFormState = CreateRaffleFormState
  { _commitDdl :: Text
  , _revealDdl :: Text
  , _ticketPrice :: Int
  , _minNoTickets :: Int
  , _availableAssets :: Data.Vector.Vector (CurrencySymbol, TokenName, Integer)
  , _selectedAsset :: Maybe (CurrencySymbol, TokenName, Integer)
  , _amount :: Int
  , _raffleRecipient :: Text
  }
  deriving (Show)

makeLenses ''CreateRaffleFormState

mkCreateRaffleForm :: CreateRaffleFormState -> Form CreateRaffleFormState e NameResources
mkCreateRaffleForm =
  newForm
    [ (txt "Commit deadline: " <+>) @@= editTextField commitDdl CommitDdlField (Just 1)
    , (txt "Reveal deadline: " <+>) @@= editTextField revealDdl RevealDdlField (Just 1)
    , (txt "Ticket price ₳ : " <+>) @@= editShowableField ticketPrice TicketPriceField
    , (txt "Min. no. of tickets: " <+>) @@= editShowableField minNoTickets MinNoTokensField
    , (txt "Select Asset" <+>) @@= listField _availableAssets selectedAsset (valueItemWidget True) 5 StakeValueListField
    , (txt "Amount: " <+>) @@= editShowableField amount StakeAmountField
    , (txt "Recipient address: " <=>) @@= editShowableFieldWithValidate raffleRecipient SendRaffleAddressField (liftA2 (||) (isJust . addressFromTextMaybe) Data.Text.null)
    ]

raffleFormToConfig :: CreateRaffleFormState -> Maybe RaffleConfig
raffleFormToConfig CreateRaffleFormState {..} = do
  cddl <- timeToPlutus <$> gyIso8601ParseM @Maybe (Data.Text.unpack _commitDdl)
  rddl <- timeToPlutus <$> gyIso8601ParseM @Maybe (Data.Text.unpack _revealDdl)
  stake <- _selectedAsset
  return $
    RaffleConfig
      { rCommitDDL = cddl
      , rRevealDDL = rddl
      , rTicketPrice = toLovelace $ fromIntegral _ticketPrice
      , rMinTickets = fromIntegral _minNoTickets
      , rStake = unFlattenValue [stake]
      }

------------------------------------------------------------------------------------------------

-- **  ActiveRaffles Form

------------------------------------------------------------------------------------------------
data ActiveRafflesFormState = ActiveRafflesFormState
  { _activeRaffles :: Data.Vector.Vector RaffleInfo
  , _selectedRaffle :: Maybe RaffleInfo
  }
  deriving (Show)

makeLenses ''ActiveRafflesFormState

drawValueWidget :: Value -> Widget NameResources
drawValueWidget val =
  vBox $
    valueItemWidget False False <$> flattenValue val

drawRaffleLockedValue :: Value -> Widget NameResources
drawRaffleLockedValue riValue = borderWithLabel (txt " CURRENT LOCKED VALUE ") $ vLimit 30 $ hLimit 70 $ drawValueWidget riValue

drawRaffeStats :: RaffleStateData -> Widget NameResources
drawRaffeStats RaffleStateData {..} =
  borderWithLabel (txt " PROGRESS ") $
    renderTable $
      table
        [ [txt "Sold Tickets: ", txt (showText rSoldTickets)]
        , [txt "Revealed Tickets: ", txt (showText rRevealedTickets)]
        , [txt "Refunded Tickets: ", txt (showText rRefundedTickets)]
        , [txt "Accumulated Random: ", txt (showText rRandomSeed)]
        ]

drawPOSIX :: PlutusLedgerApi.V1.Time.POSIXTime -> Widget NameResources
drawPOSIX = str . gyIso8601Show . timeFromPlutus

drawRaffleConfig :: RaffleConfig -> Widget NameResources
drawRaffleConfig RaffleConfig {..} =
  borderWithLabel (txt " CONFIGURATION ") $
    renderTable $
      table
        [ [txt "Commit Deadline: ", drawPOSIX rCommitDDL]
        , [txt "Reveal Deadline: ", drawPOSIX rRevealDDL]
        , [txt "Ticket Price | ₳ |: ", txt (showText $ lovelaceOf rTicketPrice)]
        , [txt "Min. Tickets: ", txt (showText rMinTickets)]
        , [txt "Raffle Stake: ", vLimit 30 $ hLimit 70 $ drawValueWidget rStake]
        ]

drawRaffleInfo :: RaffleInfo -> Widget NameResources
drawRaffleInfo RaffleInfo {..} =
  joinBorders
    <$> borderWithLabel (str (show $ rRaffleID riRsd))
    $ str riStateLabel
      <=> hBox
        ( joinBorders
            <$> [ drawRaffleConfig (rConfig riRsd)
                , drawRaffleLockedValue riValue
                , drawRaffeStats riRsd
                ]
        )

drawRaffleInfoListItem :: Bool -> RaffleInfo -> Widget NameResources
drawRaffleInfoListItem isSelected ri = if isSelected then withAttr "action" $ drawRaffleInfo ri else drawRaffleInfo ri

mkActiveRafflesForm :: ActiveRafflesFormState -> Form ActiveRafflesFormState e NameResources
mkActiveRafflesForm =
  newForm
    [(txt mempty <=>) @@= listField _activeRaffles selectedRaffle drawRaffleInfoListItem 10 ActiveRafflesListField]

------------------------------------------------------------------------------------------------

-- **  My Raffles Form

------------------------------------------------------------------------------------------------
data MyRafflesFormState = MyRafflesFormState
  { _myRaffles :: Data.Vector.Vector RaffleInfo
  , _selectedMyRaffle :: Maybe RaffleInfo
  }
  deriving (Show)

makeLenses ''MyRafflesFormState

mkMyRafflesForm :: MyRafflesFormState -> Form MyRafflesFormState e NameResources
mkMyRafflesForm =
  newForm
    [(txt mempty <=>) @@= listField _myRaffles selectedMyRaffle drawRaffleInfoListItem 10 MyRafflesListField]

------------------------------------------------------------------------------------------------

-- **  BuyTicket Form

------------------------------------------------------------------------------------------------
data BuyTicketFormState = BuyTicketFormState
  { _secret :: Text
  , _ticketRecipient :: Text
  }
  deriving (Show)

makeLenses ''BuyTicketFormState

mkBuyTicketForm :: BuyTicketFormState -> Form BuyTicketFormState e NameResources
mkBuyTicketForm =
  newForm
    [ (txt "Ticket Secret: " <=>) @@= editTextField secret SecretField (Just 5)
    , (txt "Recipient address: " <=>) @@= editShowableFieldWithValidate ticketRecipient SendTicketAddressField (liftA2 (||) (isJust . addressFromTextMaybe) Data.Text.null)
    ]

------------------------------------------------------------------------------------------------

-- **  My Tickets Form

------------------------------------------------------------------------------------------------
data MyTicketsFormState = MyTicketsFormState
  { _myTickets :: Data.Vector.Vector TicketInfo
  , _selectedTicket :: Maybe TicketInfo
  }
  deriving (Show)

makeLenses ''MyTicketsFormState

drawTicketState :: TicketStateData -> TicketStateLabel -> Widget NameResources
drawTicketState TicketStateData {..} state =
  vLimit 20 $
    hLimit 120 $
      renderTable $
        table
          [ [txt "Ticket Number: ", txt (showText tNumber)]
          , [txt "Ticket State: ", txt (showText state)]
          , [txt "Secret Hash: ", txt (showText $ toJSON tSecretHash)]
          , [txt "Revealed Secret: ", txt (showText tSecret)]
          ]

drawTicketLockedValue :: Value -> Widget NameResources
drawTicketLockedValue tiValue = borderWithLabel (txt " CURRENT LOCKED VALUE ") $ drawValueWidget tiValue

drawTicketInfo :: TicketInfo -> Widget NameResources
drawTicketInfo TicketInfo {..} =
  joinBorders
    <$> borderWithLabel
      (txt (showText (tRaffle tiTsd)))
    $ hBox
      ( joinBorders
          <$> [ drawTicketLockedValue tiValue
              , drawTicketState tiTsd tiStateLabel
              ]
      )

drawTicketInfoListItem :: Bool -> TicketInfo -> Widget NameResources
drawTicketInfoListItem isSelected ti = if isSelected then withAttr "action" $ drawTicketInfo ti else drawTicketInfo ti

mkMyTicketsForm :: MyTicketsFormState -> Form MyTicketsFormState e NameResources
mkMyTicketsForm =
  newForm
    [(txt mempty <=>) @@= listField _myTickets selectedTicket drawTicketInfoListItem 10 MyTicketsListField]

------------------------------------------------------------------------------------------------

-- *   Main State

------------------------------------------------------------------------------------------------

data Screen
  = MainScreen
  | CreateRaffleScreen
  | MintTokenScreen
  | ActiveRafflesScreen
  | MyRafflesScreen
  | BuyTicketScreen
  | MyTicketsScreen
  deriving (Eq, Ord, Enum, Show)

type RaffleizeEvent = ()

data RaffleizeUI = RaffleizeUI
  { providersCtx :: ProviderCtx
  , validatorsConfig :: Maybe RaffleizeTxBuildingContext
  , secretKey :: Maybe GYPaymentSigningKey
  , balance :: Value
  , logo :: String
  , message :: Text
  , mintTokenForm :: Form MintTokenFormState RaffleizeEvent NameResources
  , createRaffleForm :: Form CreateRaffleFormState RaffleizeEvent NameResources
  , activeRafflesForm :: Form ActiveRafflesFormState RaffleizeEvent NameResources
  , myRafflesForm :: Form MyRafflesFormState RaffleizeEvent NameResources
  , buyTicketForm :: Form BuyTicketFormState RaffleizeEvent NameResources
  , myTicketsForm :: Form MyTicketsFormState RaffleizeEvent NameResources
  , currentScreen :: Screen
  }

------------------------------------------------------------------------------------------------

-- *   App definition

------------------------------------------------------------------------------------------------

app :: App RaffleizeUI RaffleizeEvent NameResources
app =
  App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    }

tui :: IO ()
tui = do
  putStrLn $ yellowColorString $ "Parsing Config  at " <> show atlasCoreConfig <> "..."
  atlasConfig <- maybe (error $ "Valid config file not found at " <> show atlasCoreConfig) return =<< decodeConfigFile @GYCoreConfig atlasCoreConfig
  putStrLn $ yellowColorString "Loading Providers ..."
  initialState <- withCfgProviders atlasConfig "terminal user interface" $ \providers -> do
    buildInitialState providers
  _endState <- defaultMain app initialState
  return ()

------------------------------------------------------------------------------------------------

-- *  Building the initial state

------------------------------------------------------------------------------------------------

buildInitialState :: GYProviders -> IO RaffleizeUI
buildInitialState providers = do
  liftIO $ print ("BUILDING INITIAL STATE" :: String)
  logo <- readFile raffleizeLogoPath
  atlasConfig <- fromMaybe (error ("altas_config.json not defined" :: String)) <$> decodeConfigFile @GYCoreConfig atlasCoreConfig
  let pCtx = ProviderCtx atlasConfig providers
  maybeValidatorsConfig <- decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  allRafflesInfo <- getActiveRaffles pCtx
  let mintTokenForm = mkMintTokenForm (MintTokenFormState "test-tokens" 1)
  let buyTicketForm = mkBuyTicketForm (BuyTicketFormState mempty mempty)
  let activeRafflesForm = mkActiveRafflesForm (ActiveRafflesFormState (Data.Vector.fromList allRafflesInfo) Nothing)
  maybeSKey <- readPaymentKeyFile operationSkeyFilePath
  (myRafflesForm, myTicketsForm, createRaffleForm, balance) <- case maybeSKey of
    Nothing -> do
      let rf = mkMyRafflesForm (MyRafflesFormState mempty Nothing)
      let tf = mkMyTicketsForm (MyTicketsFormState mempty Nothing)
      let cf = mkCreateRaffleForm (CreateRaffleFormState mempty mempty 0 0 mempty Nothing 0 mempty)
      return (rf, tf, cf, mempty)
    Just sKey -> do
      -- let myRafflesIds = getMyRaffleIdsFromValue val
      -- let myRafflesInfo = filter (\ri -> rRaffleID (riRsd ri) `elem` myRafflesIds) allRafflesInfo
      (addr, balance) <- getAddressAndValue pCtx sKey
      myRafflesInfo <- getMyRaffles pCtx addr
      myTicketsInfo <- getMyTickets pCtx addr
      let flattenedVal = flattenValue balance
      now <- getCurrentTime
      let icd = addUTCTime 864000 now -- + 10 days
      let ird = addUTCTime 864000 icd
      let commitSuggestion = Data.Text.pack . iso8601Show $ icd
      let revealSuggestion = Data.Text.pack . iso8601Show $ ird
      let cf = mkCreateRaffleForm (CreateRaffleFormState commitSuggestion revealSuggestion 5 1 (Data.Vector.fromList flattenedVal) (listToMaybe flattenedVal) 0 mempty)
      let rf = mkMyRafflesForm (MyRafflesFormState (Data.Vector.fromList myRafflesInfo) Nothing)
      let tf = mkMyTicketsForm (MyTicketsFormState (Data.Vector.fromList myTicketsInfo) Nothing)
      return (rf, tf, cf, balance)
  return (RaffleizeUI pCtx maybeValidatorsConfig maybeSKey balance logo mempty mintTokenForm createRaffleForm activeRafflesForm myRafflesForm buyTicketForm myTicketsForm MainScreen)

------------------------------------------------------------------------------------------------

-- *  Handling Events

------------------------------------------------------------------------------------------------

handleEvent :: RaffleizeUI -> BrickEvent NameResources RaffleizeEvent -> EventM NameResources (Next RaffleizeUI)
handleEvent s e =
  case e of
    VtyEvent vtye -> case vtye of
      EvKey KEsc [] -> continue s {message = "", currentScreen = MainScreen}
      EvKey key _modifiers ->
        case currentScreen s of
          MyTicketsScreen -> do
            mtForm <- handleFormEvent e (myTicketsForm s)
            continue s {myTicketsForm = mtForm}
          BuyTicketScreen ->
            let btForm = buyTicketForm s
                btFormState = formState btForm
                secretString = Data.Text.unpack $ btFormState ^. secret
                mRecipient = addressFromTextMaybe $ btFormState ^. ticketRecipient
                arState = formState (activeRafflesForm s)
                contextNFT = rRaffleID $ riRsd $ fromJust $ arState ^. selectedRaffle
                invalid_fields = invalidFields btForm
             in case key of
                  KEnter -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        if null invalid_fields
                          then do
                            liftIO clearScreen
                            txOutRef <- liftIO $ buyTicket (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust (secretKey s)) secretString contextNFT mRecipient
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (ctxProviders (providersCtx s))
                            continue
                              initialState
                                { message = "TICKET BOUGHT SUCCESFULLY!\n" <> showLink nid "tx" txOutRef <> "\n FOR RAFFLE " <> showText contextNFT
                                , currentScreen = MainScreen
                                }
                          else continue s
                  _ -> do
                    updated_form <- handleFormEvent e btForm
                    continue s {buyTicketForm = updated_form}
          MyRafflesScreen -> do
            mrForm <- handleFormEvent e (myRafflesForm s)
            continue s {myRafflesForm = mrForm}
          ActiveRafflesScreen ->
            case key of
              KChar 'b' -> do
                let arState = formState (activeRafflesForm s)
                let isValidBuy = any ((== "BuyTicket") . snd) $ riAvailableActions $ fromJust $ arState ^. selectedRaffle
                if isValidBuy then continue s {currentScreen = BuyTicketScreen} else continue s
              _ -> do
                arForm <- handleFormEvent e (activeRafflesForm s)
                continue s {activeRafflesForm = arForm}
          CreateRaffleScreen ->
            let crForm = createRaffleForm s
                invalid_fields = invalidFields crForm
             in case key of
                  KEnter -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        let mraffle = raffleFormToConfig (formState crForm)
                        if null invalid_fields && isJust mraffle
                          then do
                            liftIO clearScreen

                            let raffle = fromJust mraffle
                            let recpient = addressFromTextMaybe $ _raffleRecipient (formState crForm)
                            txOutRef <- liftIO $ createRaffle (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust (secretKey s)) raffle recpient
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (ctxProviders (providersCtx s))
                            continue initialState {message = "RAFFLE SUCCESFULLY CREATED !\n" <> showLink nid "tx" txOutRef}
                          else continue s
                  _ -> do
                    crForm1 <- handleFormEvent e crForm
                    let crFormState1 = formState crForm1
                    let selectedElementAmount = maybe (0 :: Int) (\(_, _, i) -> fromIntegral i) (crFormState1 ^. selectedAsset)
                    let crForm2 =
                          if key `elem` [KUp, KDown]
                            then do
                              let currentAmount = crFormState1 ^. amount
                              let updated_form_state_again = crFormState1 {_amount = selectedElementAmount}
                              if selectedElementAmount /= currentAmount then updateFormState updated_form_state_again crForm1 else crForm1
                            else crForm1
                    let crFormState2 = formState crForm2
                    let fieldValidations =
                          [ setFieldValid (isJust $ gyIso8601ParseM @Maybe (Data.Text.unpack (crFormState2 ^. commitDdl))) CommitDdlField
                          , setFieldValid (isJust $ gyIso8601ParseM @Maybe (Data.Text.unpack (crFormState2 ^. revealDdl))) RevealDdlField
                          , setFieldValid (crFormState2 ^. ticketPrice > (fromIntegral (rMinTicketPrice mockRaffleParam) `div` 1000000)) TicketPriceField
                          , setFieldValid (liftA2 (&&) (> 0) (< fromIntegral (rMaxNoOfTickets mockRaffleParam)) $ crFormState2 ^. minNoTickets) MinNoTokensField
                          , setFieldValid (crFormState2 ^. amount <= selectedElementAmount) StakeAmountField
                          ]
                    let validated_form = foldr' ($) crForm2 fieldValidations
                    continue s {createRaffleForm = validated_form}
          MintTokenScreen ->
            let mtForm = mintTokenForm s
                mtFormState = formState mtForm
                mtTokenNameField = mtFormState ^. tokenNameField
                mtAmountField = mtFormState ^. mintAmount
                invalid_fields = invalidFields mtForm
             in case key of
                  KEnter ->
                    do
                      if null invalid_fields
                        then do
                          liftIO clearScreen
                          let mySkey = fromJust (secretKey s)
                          let myAddr = addressFromSkey (providersCtx s) mySkey
                          txOutRef <-
                            liftIO $
                              mintTestTokens
                                (providersCtx s)
                                mySkey
                                myAddr
                                (Data.Text.unpack mtTokenNameField)
                                (fromIntegral mtAmountField)
                          let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                          continue
                            s
                              { message = "TEST TOKENS SUCCESFULLY MINTED !\n" <> showLink nid "tx" txOutRef
                              , currentScreen = MainScreen
                              }
                        else continue s
                  _ -> do
                    updated_form <- handleFormEvent e mtForm
                    let newMtFormState = formState mtForm
                    let newMtTokenNameField = newMtFormState ^. tokenNameField
                    let newMtAmountField = newMtFormState ^. mintAmount
                    let fieldValidations =
                          [ setFieldValid (Data.Text.length newMtTokenNameField <= tokenNameMaxLength) TokenNameField
                          , setFieldValid (newMtAmountField > 0) MintAmountField
                          ]
                    let validated_form = foldr' ($) updated_form fieldValidations
                    continue s {mintTokenForm = validated_form}
          MainScreen -> case key of
            KDown -> do
              let vscroll = viewportScroll ValueItemsViewPort
              vScrollBy vscroll 1
              continue s
            KUp -> do
              let vscroll = viewportScroll ValueItemsViewPort
              vScrollBy vscroll (-1)
              continue s
            (KChar c) -> case c of
              'q' -> halt s
              'R' -> continue s {message = "Refresh Screen"}
              'g' -> do
                liftIO $ generateNewAdminSkey operationSkeyFilePath
                s' <- liftIO $ buildInitialState (ctxProviders (providersCtx s))
                continue s'
              'e' -> do
                liftIO $ sequence_ [exportRaffleScript, exportTicketScript, exportMintingPolicy]
                continue s {message = "VALIDATORS SUCCESFULLY EXPORTED !\n" <> Data.Text.pack (Data.List.intercalate "\n" [raffleizeValidatorFile, ticketValidatorFile, mintingPolicyFile])}
              'v' -> continue s {currentScreen = ActiveRafflesScreen}
              _ ->
                if isJust (secretKey s)
                  then do
                    case c of
                      'm' -> continue s {currentScreen = MintTokenScreen}
                      'c' -> do
                        continue s {currentScreen = CreateRaffleScreen}
                      'l' -> do
                        liftIO clearScreen
                        initialState <- liftIO $ buildInitialState (ctxProviders (providersCtx s))
                        continue initialState {message = "Updated"}
                      'd' -> do
                        liftIO clearScreen
                        liftIO $ deployValidators (providersCtx s) (fromJust (secretKey s))
                        initialState <- liftIO $ buildInitialState (ctxProviders (providersCtx s))
                        continue initialState {message = "VALIDATORS SUCCESFULLY DEPLOYED !\nTxOuts references are saved to " <> showText raffleizeValidatorsConfig}
                      'r' -> do
                        continue s {currentScreen = MyRafflesScreen}
                      't' -> do
                        continue s {currentScreen = MyTicketsScreen}
                      _ -> continue s
                  else continue s
            _ -> continue s
      _ -> continue s
    _ -> continue s

------------------------------------------------------------------------------------------------

-- *  Drawing

------------------------------------------------------------------------------------------------

drawUI :: RaffleizeUI -> [Widget NameResources]
drawUI s =
  joinBorders . withBorderStyle unicode . borderWithLabel (txt " RAFFLEIZE - C.A.R.D.A.N.A ")
    <$> [ if Data.Text.null (message s) then emptyWidget else center (withAttr "highlight" $ txt (message s)) <=> txt "[ESC] - Close"
        , if currentScreen s == MintTokenScreen then mintTestTokensScreen s else emptyWidget
        , if currentScreen s == CreateRaffleScreen then createRaffleScreen s else emptyWidget
        , if currentScreen s == BuyTicketScreen then buyTicketScreen s else emptyWidget
        , if currentScreen s == ActiveRafflesScreen then drawActiveRafflesScreen s else emptyWidget
        , if currentScreen s == MyRafflesScreen then drawMyRafflesScreen s else emptyWidget
        , if currentScreen s == MyTicketsScreen then drawMyTicketsScreen s else emptyWidget
        , mainScreen s
        ]

------------------------------------------------------------------------------------------------

-- **  Main Screen

------------------------------------------------------------------------------------------------

mainScreen :: RaffleizeUI -> Widget NameResources
mainScreen s =
  vBox
    [ hCenter $ withAttr "highlight" $ str (logo s)
    , hBorder
    , center $ bodyWidget s
    ]

bodyWidget :: RaffleizeUI -> Widget NameResources
bodyWidget s =
  vBox $
    padLeftRight 1
      <$> [ summaryWidget s
          , hBorder
          , assetsWidget (cfgNetworkId ((ctxCoreCfg . providersCtx) s)) (secretKey s) (balance s)
          ]

summaryWidget :: RaffleizeUI -> Widget NameResources
summaryWidget s =
  hCenter $
    hBox
      [ availableActionsWidget s
      , providersWidget ((ctxCoreCfg . providersCtx) s)
      , validatorsWidget (cfgNetworkId ((ctxCoreCfg . providersCtx) s)) (validatorsConfig s)
      , walletFlagWidget (secretKey s)
      ]

symbolWidget :: Bool -> Widget n
symbolWidget v = if v then withAttr "good" $ txt "✔" else withAttr "warning" $ txt "X"

validatorsWidget :: GYNetworkId -> Maybe RaffleizeTxBuildingContext -> Widget n
validatorsWidget nid mv =
  borderWithLabel (txt "VALIDATORS") $
    renderTable $
      table $
        [txt "Deployed: ", symbolWidget (isJust mv)] : case mv of
          (Just (RaffleizeTxBuildingContext {..})) ->
            [ [txt "Raffle Validator", txOutRefWidget nid raffleValidatorRef]
            , [txt "Ticket Validator", txOutRefWidget nid ticketValidatorRef]
            ]
          _ -> []

walletFlagWidget :: Maybe a -> Widget NameResources
walletFlagWidget mkey =
  borderWithLabel (txt "WALLET") $
    renderTable $
      table
        [[txt "Connected ", symbolWidget (isJust mkey)]]

txOutRefWidget :: GYNetworkId -> GYTxOutRef -> Widget n
txOutRefWidget nid t =
  let txoutrefText = showTxOutRef t
   in hyperlink (showLink nid "tx" txoutrefText) $ withAttr "good" $ txt txoutrefText

providersWidget :: GYCoreConfig -> Widget n
providersWidget cfg =
  borderWithLabel (txt "BLOCKCHAIN PROVIDER") $
    renderTable $
      table
        [ [txt "Loaded: ", symbolWidget True]
        , [txt "Provider: ", printProvider cfg]
        , [txt "Network: ", printNetwork cfg]
        ]

printProvider :: GYCoreConfig -> Widget n
printProvider cfg = withAttr "good" . txt $ case cfgCoreProvider cfg of
  (GYNodeKupo {}) -> "KUPO"
  (GYMaestro {}) -> "MAESTRO"
  (GYBlockfrost {}) -> "BLOCKFROST"

printNetwork :: GYCoreConfig -> Widget n
printNetwork cfg = withAttr "good" . txt $ case cfgNetworkId cfg of
  GYMainnet -> "MAINNET"
  GYTestnetPreprod -> "PREPROD"
  GYTestnetPreview -> "PREVIEW"
  GYTestnetLegacy -> "TESTNET-LEGACY"
  GYPrivnet -> "PRIVNET"

adaBalanceWidget :: Value -> Widget n
adaBalanceWidget val = withAttr "good" $ txt (showText (fromValue val) <> "\n")

instance Splittable [] where
  splitAt :: Int -> [a] -> ([a], [a])
  splitAt i = Prelude.splitAt (fromIntegral i)

assetsBalanceWidget :: Value -> Widget NameResources
assetsBalanceWidget val =
  let valueItemsList = Brick.Widgets.List.list ValueItemsList (flattenValue val) 5
   in vLimit 100 $
        hLimit 150 $
          borderWithLabel (txt "ASSETS") $
            visible $
              withVScrollBarHandles $
                withVScrollBars OnRight $
                  viewport ValueItemsViewPort Vertical $
                    vLimit 300 $
                      hLimit 150 $
                        renderList (valueItemWidget False) False valueItemsList

addressWidget :: GYNetworkId -> GYAddress -> Widget n
addressWidget nid addr =
  let addrText = addressToText addr
   in hyperlink (showLink nid "address" addrText) $ withAttr "good" $ txt addrText

assetsWidget :: GYNetworkId -> Maybe GYPaymentSigningKey -> Value -> Widget NameResources
assetsWidget nid (Just skey) val =
  let addr = addressFromPaymentSigningKey nid skey
   in borderWithLabel (txt "WALLET") $
        hBox
          [ assetsAdaWidget nid addr val
          , vBorder
          , assetsBalanceWidget val
          ]
assetsWidget _ _ _ = emptyWidget

assetsAdaWidget :: GYNetworkId -> GYAddress -> Value -> Widget NameResources
assetsAdaWidget nid addr val =
  renderTable $
    table
      [ [txt "Address: ", addressWidget nid addr]
      , [txt "Ada | ₳ |:", adaBalanceWidget val]
      ]

availableActionsWidget :: RaffleizeUI -> Widget n
availableActionsWidget s =
  withAttr "action" . borderWithLabel (txt "AVAILABLE ACTIONS") $
    vBox
      ( txt
          <$> filter
            (not . Data.Text.null)
            ( ( if isNothing (secretKey s)
                  then ["[G] - Generate new wallet skey"]
                  else
                    [ "[V] - View all active raffles"
                    , "[R] - View my raffles"
                    , "[T] - View my tickets"
                    , "[C] - Create raffle" -- TODO : CHECK ADA BALANCE
                    , "[M] - Mint some test tokens"
                    , "[D] - Deploy Raffleize Validators"
                    , "[L] - Get wallet balance"
                    ]
              )
                ++ [ "[E] - Export validators"
                   , "[Q] - Quit"
                   ]
            )
      )

------------------------------------------------------------------------------------------------

-- **  Active Raffles Screen

------------------------------------------------------------------------------------------------

drawRaffleActionsWidget :: Maybe [RaffleizeActionLabel] -> Widget NameResources
drawRaffleActionsWidget mActions =
  withAttr "action" $
    borderWithLabel (txt "AVAILABLE ACTIONS") $
      vBox $
        maybe [] (drawRaffleActionLabel <$>) mActions
          ++ [txt "[ESC]   - Close          "]

drawRaffleActionLabel :: RaffleizeActionLabel -> Widget NameResources
drawRaffleActionLabel (_, "BuyTicket") = txt "[B]     - Buy a ticket for the selected raffle"
drawRaffleActionLabel _ = emptyWidget

drawActiveRafflesScreen :: RaffleizeUI -> Widget NameResources
drawActiveRafflesScreen s =
  let arForm = activeRafflesForm s
      arFormState = formState arForm
      currentActions = riAvailableActions <$> (arFormState ^. selectedRaffle)
   in center $
        borderWithLabel (txt " ACTIVE RAFFLES ") $
          vBox $
            padAll 1
              <$> [ center $ renderForm arForm
                  , hBorder
                  , hCenter $ drawRaffleActionsWidget currentActions
                  ]

------------------------------------------------------------------------------------------------

-- **  My Tickets Screen

------------------------------------------------------------------------------------------------

drawMyTicketsScreen :: RaffleizeUI -> Widget NameResources
drawMyTicketsScreen s =
  let mtForm = myTicketsForm s
      mtFormState = formState mtForm
      currentActions = tiAvailableActions <$> (mtFormState ^. selectedTicket)
   in center $
        borderWithLabel (txt " MY TICKETS ") $
          vBox $
            padAll 1
              <$> [ center $ renderForm mtForm
                  , hBorder
                  , hCenter $ drawMyTicketsActionsWidget currentActions
                  ]

drawMyTicketsActionsWidget :: Maybe [RaffleizeActionLabel] -> Widget NameResources
drawMyTicketsActionsWidget mActions =
  withAttr "action" $
    borderWithLabel (txt "AVAILABLE ACTIONS") $
      vBox $
        maybe [] (drawMyTicketActionLabel <$>) mActions
          ++ [txt "[ESC]   - Close          "]

drawMyTicketActionLabel :: RaffleizeActionLabel -> Widget NameResources
drawMyTicketActionLabel label = case label of
  ("TicketOwner", "RefundTicket") -> txt "[R]     - Get full refund"
  ("TicketOwner", "RevealTicketSecret") -> txt "[S]     - Reveal the ticket secret"
  ("TicketOwner", "CollectStake") -> txt "[W]     - Redeem the winning ticket"
  ("TicketOwner", "RefundCollateralLosing") -> txt "[L]     - Get refund of the collateral on losing ticket"
  ("TicketOwner", "RefundTicketExtra") -> txt "[E]     - Get ticket refund and extra"
  ("RaffleOwner", "GetCollateraOfExpiredTicket") -> emptyWidget
  _ -> emptyWidget

------------------------------------------------------------------------------------------------

-- **  My Raffles Screen

------------------------------------------------------------------------------------------------

drawMyRafflesScreen :: RaffleizeUI -> Widget NameResources
drawMyRafflesScreen s =
  let mrForm = myRafflesForm s
      mrFormState = formState mrForm
      currentActions = riAvailableActions <$> (mrFormState ^. selectedMyRaffle)
   in center $
        borderWithLabel (txt " MY RAFFLES ") $
          vBox $
            padAll 1
              <$> [ center $ renderForm mrForm
                  , hBorder
                  , hCenter $ drawMyRaffleActionsWidget currentActions
                  ]

drawMyRaffleActionsWidget :: Maybe [RaffleizeActionLabel] -> Widget NameResources
drawMyRaffleActionsWidget mActions =
  withAttr "action" $
    borderWithLabel (txt "AVAILABLE ACTIONS") $
      vBox $
        maybe [] (drawMyRaffleActionLabel <$>) mActions
          ++ [txt "[ESC]   - Close          "]

drawMyRaffleActionLabel :: RaffleizeActionLabel -> Widget NameResources
drawMyRaffleActionLabel ("RaffleOwner", what) = txt (Data.Text.pack what)
drawMyRaffleActionLabel _ = emptyWidget

------------------------------------------------------------------------------------------------

-- **  Create Raffle Screen

------------------------------------------------------------------------------------------------

createRaffleScreen :: RaffleizeUI -> Widget NameResources
createRaffleScreen = center . mkFormScreen " CREATE NEW RAFFLE " " CREATE NEW RAFFLE " createRaffleForm

------------------------------------------------------------------------------------------------

-- **  Mint Test Tokens Screen

------------------------------------------------------------------------------------------------

mintTestTokensScreen :: RaffleizeUI -> Widget NameResources
mintTestTokensScreen = center . mkFormScreen " MINT TEST TOKENS " " MINT TEST TOKENS " mintTokenForm

------------------------------------------------------------------------------------------------

-- **  Mint Test Tokens Screen

------------------------------------------------------------------------------------------------

buyTicketScreen :: RaffleizeUI -> Widget NameResources
buyTicketScreen s =
  let arState = formState (activeRafflesForm s)
      contextNFT = rRaffleID $ riRsd $ fromJust $ arState ^. selectedRaffle
   in center $ mkFormScreen (" BUY TICKET " <> showText contextNFT) " BUY TICKET " buyTicketForm s

------------------------------------------------------------------------------------------------

-- **  Form Screens

------------------------------------------------------------------------------------------------

mkFormScreen :: Text -> Text -> (RaffleizeUI -> Form s e NameResources) -> RaffleizeUI -> Widget NameResources
mkFormScreen title actionsDesc raffleizeForm s =
  let ivfs = invalidFields (raffleizeForm s)
   in borderWithLabel (txt title) $
        vBox $
          padAll 1
            <$> [ withAttr formAttr (withAttr invalidFormInputAttr (withAttr focusedFormInputAttr (renderForm $ raffleizeForm s)))
                , hBorder
                , hCenter $ invalidFieldsWidget ivfs
                , hBorder
                , hCenter $ formActionsWidget (null ivfs) actionsDesc
                ]

formActionsWidget :: Bool -> Text -> Widget n
formActionsWidget isValid s =
  withAttr "action" . borderWithLabel (txt "AVAILABLE ACTIONS") $
    vBox
      [ txt "[ESC]   - Close          "
      , if isValid then txt ("[Enter] - " <> s) else emptyWidget
      ]

invalidFieldText :: NameResources -> Text
invalidFieldText t = case t of
  TokenNameField -> "Token name must have maximum " <> showText tokenNameMaxLength <> " characters!"
  MintAmountField -> "The minting amount must be a natural number!"
  CommitDdlField -> "Commit deadline must be yyyy-mm-ddThh:mm:ss[.ss]Z (eg. 1970-01-01T00:00:00Z"
  RevealDdlField -> "Reveal deadline must be yyyy-mm-ddThh:mm:ss[.ss]Z (eg. 1970-01-01T00:00:00Z"
  MinNoTokensField -> "The minimum number of tokens must be a natural number, lower than " <> showText (rMaxNoOfTickets mockRaffleParam) <> " !"
  StakeAmountField -> "The amount field must be lower or equal to the available amount of the selected asset"
  TicketPriceField -> "The ticket price must be a natural number, lower than " <> showText (rMinTicketPrice mockRaffleParam) <> " !"
  SendRaffleAddressField -> "Enter a valid address or leave empty to receive the raffle NFT to the current address"
  SendTicketAddressField -> "Enter a valid address or leave empty to receive the ticket to the current address"
  _ -> ""

invalidFieldsWidget :: [NameResources] -> Widget n
invalidFieldsWidget ivfs = withAttr "warning" $ txt (Data.Text.intercalate "\n" $ invalidFieldText <$> ivfs)

tokenNameMaxLength :: Int
tokenNameMaxLength = 32

------------------------------------------------------------------------------------------------

-- **  Styling

------------------------------------------------------------------------------------------------

theMap :: AttrMap
theMap =
  attrMap
    (white `on` black)
    [ ("highlight", fg magenta)
    , ("warning", fg red)
    , ("good", fg green)
    , ("action", fg yellow)
    , ("selected", bg blue)
    , (focusedFormInputAttr, fg brightBlue)
    , (invalidFormInputAttr, white `on` red)
    ]

---------
