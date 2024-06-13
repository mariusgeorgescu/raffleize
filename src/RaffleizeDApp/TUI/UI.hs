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

import Data.Maybe (catMaybes, fromMaybe)

import Brick.Focus (FocusRing, focusGetCurrent, focusNext, focusRing)
import Codec.Serialise.Internal.GeneralisedUTF8 ()
import GHC.Real
import RaffleizeDApp.OnChain.RaffleizeLogic
import RaffleizeDApp.OnChain.Utils (showValue)
import RaffleizeDApp.Tests.UnitTests (yellowColorString)
import RaffleizeDApp.TxBuilding.Context
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators
import System.Console.ANSI (clearScreen)
import System.IO.Extra (readFile)

instance Splittable [] where
  splitAt :: Int -> [a] -> ([a], [a])
  splitAt i = Prelude.splitAt (fromIntegral i)

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
type RaffleizeEvent = ()

data NameResources
  = CommitDdlField
  | RevealDdlField
  | MinNoTokensField
  | AddToValueListField
  | AddToValueAmountField
  | TicketPriceField
  | ActiveRafflesListField
  | MyRafflesListField
  | MyTicketsListField
  | ValueItemsList
  | AvailableValueItemsList
  | ConstructedValueItemsList
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

-- **  Add To Stake Form

------------------------------------------------------------------------------------------------

data AddToValueFormState = AddToValueFormState
  { _amount :: Int
  }
  deriving (Show)

makeLenses ''AddToValueFormState

mkAddToStakeFormState :: AddToValueFormState -> Form AddToValueFormState e NameResources
mkAddToStakeFormState =
  newForm
    [(txt "Amount: " <=>) @@= editShowableField amount AddToValueAmountField]

------------------------------------------------------------------------------------------------

-- **  Create new raffle form

------------------------------------------------------------------------------------------------

data RaffleConfigFormState = RaffleConfigFormState
  { _commitDdl :: Text
  , _revealDdl :: Text
  , _ticketPrice :: Int
  , _minNoTickets :: Int
  , _raffleRecipient :: Text
  }
  deriving (Show)

makeLenses ''RaffleConfigFormState

mkRaffleConfigForm :: RaffleConfigFormState -> Form RaffleConfigFormState e NameResources
mkRaffleConfigForm =
  newForm
    [ (txt "Commit deadline: " <+>) @@= editTextField commitDdl CommitDdlField (Just 1)
    , (txt "Reveal deadline: " <+>) @@= editTextField revealDdl RevealDdlField (Just 1)
    , (txt "Ticket price ₳ : " <+>) @@= editShowableField ticketPrice TicketPriceField
    , (txt "Min. no. of tickets: " <+>) @@= editShowableField minNoTickets MinNoTokensField
    , (txt "Recipient address: " <+>) @@= editShowableFieldWithValidate raffleRecipient SendRaffleAddressField (liftA2 (||) (isJust . addressFromTextMaybe) Data.Text.null)
    ]

raffleFormToConfig :: RaffleConfigFormState -> Value -> Maybe RaffleConfig
raffleFormToConfig RaffleConfigFormState {..} stake = do
  cddl <- timeToPlutus <$> gyIso8601ParseM @Maybe (Data.Text.unpack _commitDdl)
  rddl <- timeToPlutus <$> gyIso8601ParseM @Maybe (Data.Text.unpack _revealDdl)

  return $
    RaffleConfig
      { rCommitDDL = cddl
      , rRevealDDL = rddl
      , rTicketPrice = toLovelace $ fromIntegral _ticketPrice
      , rMinTickets = fromIntegral _minNoTickets
      , rStake = stake
      }

------------------------------------------------------------------------------------------------

-- CONFIGSTAKE

------------------------------------------------------------------------------------------------

data ConstructValueState = ConstructValueState
  { addToValueForm :: Form AddToValueFormState RaffleizeEvent NameResources
  , availableValueList :: GenericList NameResources [] (CurrencySymbol, TokenName, Integer)
  , constructedValueList :: GenericList NameResources [] (CurrencySymbol, TokenName, Integer)
  , elementFocus :: FocusRing Integer
  }

mkConstructValueState :: Value -> Value -> ConstructValueState
mkConstructValueState available constructed =
  ConstructValueState
    { addToValueForm = mkAddToStakeFormState (AddToValueFormState 1)
    , availableValueList = Brick.Widgets.List.list AvailableValueItemsList (flattenValue available) 5
    , constructedValueList = Brick.Widgets.List.list ConstructedValueItemsList (flattenValue constructed) 5
    , elementFocus = focusRing [1, 2, 3]
    }

drawConstructValueWidget :: ConstructValueState -> Widget NameResources
drawConstructValueWidget ConstructValueState {addToValueForm, availableValueList, constructedValueList, elementFocus} =
  let
    ivfs = invalidFields addToValueForm
    isValidToAdd = null $ invalidFields addToValueForm
    currentFocus = Data.Maybe.fromMaybe 1 $ focusGetCurrent elementFocus
   in
    center $
      borderWithLabel (txt " CONFIGURE RAFFLE VALUE ") $
        hBox
          ( padAll
              1
              <$> [ (if currentFocus == 1 then withAttr focusedFormInputAttr else id)
                      (renderList (valueItemWidget True) (currentFocus == 2) availableValueList)
                      <=> (if currentFocus == 2 then withAttr focusedFormInputAttr else id) (renderForm addToValueForm)
                  , vBorder
                  , txt "Current Selected Value " <=> (if currentFocus /= 3 then id else withAttr focusedFormInputAttr) (renderList (valueItemWidget True) (currentFocus == 2) constructedValueList)
                  ]
          )
          <=> vBox
            [ if isValidToAdd then emptyWidget else hBorder <=> hCenter (invalidFieldsWidget ivfs) <=> hBorder
            , constructValueActionsWidget isValidToAdd (null constructedValueList) currentFocus
            ]

constructValueActionsWidget :: Bool -> Bool -> Integer -> Widget n
constructValueActionsWidget isValidToAdd isEmptyStake currentFocus =
  withAttr "action" . borderWithLabel (txt "AVAILABLE ACTIONS") $
    vBox
      [ txt "[ESC]     - Close          "
      , if currentFocus `elem` [1, 2] then (if isValidToAdd then txt "[Insert]  - Add to value" else emptyWidget) else (if isEmptyStake then emptyWidget else txt "[Delete]  - Remove from value")
      , if not isEmptyStake then txt ("[Enter]  - " <> " Finish value construction") else emptyWidget
      ]

tokenNameMaxLength :: Int
tokenNameMaxLength = 32

invalidFieldText :: NameResources -> Text
invalidFieldText t = case t of
  TokenNameField -> "Token name must have maximum " <> showText tokenNameMaxLength <> " characters!"
  MintAmountField -> "The minting amount must be a natural number!"
  CommitDdlField -> "Commit deadline must be yyyy-mm-ddThh:mm:ss[.ss]Z (eg. 1970-01-01T00:00:00Z !"
  RevealDdlField -> "Reveal deadline must be yyyy-mm-ddThh:mm:ss[.ss]Z (eg. 1970-01-01T00:00:00Z !"
  MinNoTokensField -> "The minimum number of tokens must be a natural number, lower than " <> showText (rMaxNoOfTickets mockRaffleParam) <> " !"
  AddToValueAmountField -> "The amount field must be a natural number lower or equal to the available amount of the selected asset !"
  TicketPriceField -> "The ticket price must be a natural number, lower than " <> showText (rMinTicketPrice mockRaffleParam) <> " !"
  SendRaffleAddressField -> "Enter a valid address or leave empty to receive the raffle NFT to the current address !"
  SendTicketAddressField -> "Enter a valid address or leave empty to receive the ticket to the current address !"
  ConstructedValueItemsList -> "Stake value must not be empty !"
  _ -> ""

invalidFieldsWidget :: [NameResources] -> Widget n
invalidFieldsWidget ivfs = withAttr "warning" $ txt (Data.Text.intercalate "\n" $ invalidFieldText <$> ivfs)

drawAction :: (Text, Text) -> Widget n
drawAction (bkey, bdesc) = txt $ "[ " <> bkey <> " ]" <> "  -  " <> bdesc

drawAvailableActions :: [Maybe (Text, Text)] -> Widget n
drawAvailableActions mactions =
  withAttr "action" . borderWithLabel (txt "AVAILABLE ACTIONS") $
    vBox $
      drawAction <$> catMaybes mactions

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
drawRaffleLockedValue riValue = borderWithLabel (txt " CURRENT LOCKED VALUE ") $ vLimit 30 $ hLimit 60 $ drawValueWidget riValue

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
        , [txt "Raffle Stake: ", vLimit 30 $ hLimit 60 $ drawValueWidget rStake]
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
  | ConstructValueScreen
  deriving (Eq, Ord, Enum, Show)

data RaffleizeUI = RaffleizeUI
  { providersCtx :: ProviderCtx
  , validatorsConfig :: Maybe RaffleizeTxBuildingContext
  , secretKey :: Maybe GYPaymentSigningKey
  , balance :: Value
  , balanceList :: GenericList NameResources [] (CurrencySymbol, TokenName, Integer)
  , logo :: String
  , message :: Text
  , mintTokenForm :: Form MintTokenFormState RaffleizeEvent NameResources
  , raffleConfigForm :: Form RaffleConfigFormState RaffleizeEvent NameResources
  , activeRafflesForm :: Form ActiveRafflesFormState RaffleizeEvent NameResources
  , myRafflesForm :: Form MyRafflesFormState RaffleizeEvent NameResources
  , buyTicketForm :: Form BuyTicketFormState RaffleizeEvent NameResources
  , myTicketsForm :: Form MyTicketsFormState RaffleizeEvent NameResources
  , myConstrctValueState :: ConstructValueState
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
  atlasConfig <- Data.Maybe.fromMaybe (error ("altas_config.json not defined" :: String)) <$> decodeConfigFile @GYCoreConfig atlasCoreConfig
  let pCtx = ProviderCtx atlasConfig providers
  maybeValidatorsConfig <- decodeConfigFile @RaffleizeTxBuildingContext raffleizeValidatorsConfig
  allRafflesInfo <- getActiveRaffles pCtx
  let mintTokenForm = mkMintTokenForm (MintTokenFormState "test-tokens" 1)
  let buyTicketForm = mkBuyTicketForm (BuyTicketFormState mempty mempty)
  let activeRafflesForm = mkActiveRafflesForm (ActiveRafflesFormState (Data.Vector.fromList allRafflesInfo) Nothing)
  maybeSKey <- readPaymentKeyFile operationSkeyFilePath
  (myRafflesForm, myTicketsForm, raffleConfigForm, balance) <- case maybeSKey of
    Nothing -> do
      let rf = mkMyRafflesForm (MyRafflesFormState mempty Nothing)
      let tf = mkMyTicketsForm (MyTicketsFormState mempty Nothing)
      let cf = mkRaffleConfigForm (RaffleConfigFormState mempty mempty 0 0 mempty)
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
      let cf = mkRaffleConfigForm (RaffleConfigFormState commitSuggestion revealSuggestion 5 1 mempty)
      let rf = mkMyRafflesForm (MyRafflesFormState (Data.Vector.fromList myRafflesInfo) Nothing)
      let tf = mkMyTicketsForm (MyTicketsFormState (Data.Vector.fromList myTicketsInfo) Nothing)
      return (rf, tf, cf, balance)
  let balanceList = Brick.Widgets.List.list ValueItemsList (flattenValue balance) 5
  let constructValueState = mkConstructValueState balance mempty
  return (RaffleizeUI pCtx maybeValidatorsConfig maybeSKey balance balanceList logo mempty mintTokenForm raffleConfigForm activeRafflesForm myRafflesForm buyTicketForm myTicketsForm constructValueState MainScreen)

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
          ConstructValueScreen -> do
            let currentCVS = myConstrctValueState s
            let atvForm = addToValueForm currentCVS
            let isValidValue = allFieldsValid atvForm
            let atvFormState = formState atvForm
            let currentFocus = focusGetCurrent $ elementFocus currentCVS
            case key of
              KEnter ->
                if isValidValue && not (null (constructedValueList currentCVS))
                  then continue s {currentScreen = CreateRaffleScreen}
                  else continue s
              KChar '\t' ->
                do
                  let nextFocus = focusNext (elementFocus currentCVS)
                  let updatedCVS = currentCVS {elementFocus = nextFocus}
                  continue s {myConstrctValueState = updatedCVS}
              KIns -> do
                let selectedElement = snd <$> listSelectedElement (availableValueList currentCVS)
                case (selectedElement, isValidValue, (`elem` [1, 2]) <$> currentFocus) of
                  (Just (cs, tn, total), True, Just True) -> do
                    let currentAmount = formState atvForm ^. amount
                    let el = (cs, tn, min total (fromIntegral currentAmount))
                    let elementValue = unFlattenValue [el]
                    let availableBalance = unFlattenValue $ toList $ listElements (availableValueList currentCVS)
                    let newAvailableBalance = availableBalance #- elementValue
                    let newAvailableList = listReplace (Data.List.reverse $ flattenValue newAvailableBalance) (Just 0) (availableValueList currentCVS)
                    let stakeBalance = unFlattenValue $ toList $ listElements (constructedValueList currentCVS)
                    let newStakeBalance = stakeBalance #+ elementValue
                    let newConstructedList = listReplace (Data.List.reverse $ flattenValue newStakeBalance) (Just 0) (constructedValueList currentCVS)
                    let atvForm1 = updateFormState atvFormState {_amount = 1} atvForm
                    let newcvs = currentCVS {constructedValueList = newConstructedList, availableValueList = newAvailableList, addToValueForm = atvForm1}
                    continue s {myConstrctValueState = newcvs}
                  _ -> continue s
              _ -> do
                case currentFocus of
                  Just 1 -> do
                    newAvailableList <- handleListEvent vtye (availableValueList currentCVS)
                    let newSelectedElement = snd <$> listSelectedElement newAvailableList
                    let mselectedElementAmount = (\(_, _, i) -> fromIntegral i) <$> newSelectedElement
                    case mselectedElementAmount of
                      Nothing -> continue s
                      Just selectedElementAmount -> do
                        let atvForm1 =
                              if key `elem` [KUp, KDown]
                                then do
                                  let currentAmount = atvFormState ^. amount
                                  let atvFormState1 = atvFormState {_amount = selectedElementAmount}
                                  if selectedElementAmount /= currentAmount then updateFormState atvFormState1 atvForm else atvForm
                                else atvForm
                        let fieldValidations =
                              [ setFieldValid (null newAvailableList || liftA2 (&&) (> 0) (<= selectedElementAmount) (formState atvForm1 ^. amount)) AddToValueAmountField
                              ]
                        let validated_form = foldr' ($) atvForm1 fieldValidations
                        let newcvs = currentCVS {availableValueList = newAvailableList, addToValueForm = validated_form}
                        continue s {myConstrctValueState = newcvs}
                  Just 2 -> do
                    atvForm1 <- handleFormEvent e (addToValueForm currentCVS)
                    let atvFormState1 = formState atvForm1
                    let avl = availableValueList currentCVS
                    let newSelectedElement = snd <$> listSelectedElement avl
                    let selectedElementAmount = maybe (0 :: Int) (\(_, _, i) -> fromIntegral i) newSelectedElement
                    let fieldValidations =
                          [ setFieldValid (null avl || liftA2 (&&) (> 0) (<= selectedElementAmount) (formState atvForm1 ^. amount)) AddToValueAmountField
                          ]
                    let validated_form = foldr' ($) atvForm1 fieldValidations
                    let newcvs = currentCVS {addToValueForm = validated_form}
                    continue s {myConstrctValueState = newcvs}
                  Just 3 -> case key of
                    KDel -> do
                      let selectedElement = listSelectedElement (constructedValueList currentCVS)
                      case selectedElement of
                        Nothing -> continue s
                        Just (idx, el) -> do
                          let newConstructedList = listRemove idx (constructedValueList currentCVS)
                          let elementValue = unFlattenValue [el]
                          let availableBalance = unFlattenValue $ toList $ listElements (availableValueList currentCVS)
                          let newAvailableBalance = availableBalance #+ elementValue
                          let newAvailableList = listReplace (Data.List.reverse $ flattenValue newAvailableBalance) (Just 0) (availableValueList currentCVS)
                          let newcvs = currentCVS {constructedValueList = newConstructedList, availableValueList = newAvailableList}
                          continue s {myConstrctValueState = newcvs}
                    _ -> do
                      newConstructedList <- handleListEvent vtye (constructedValueList currentCVS)
                      let newcvs = currentCVS {constructedValueList = newConstructedList}
                      continue s {myConstrctValueState = newcvs}
                  _ -> error "invalid focus"
          MyTicketsScreen -> do
            let mtForm = myTicketsForm s
                mtFormState = formState mtForm
                mst = mtFormState ^. selectedTicket
             in case key of
                  KChar 's' -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        case mst of
                          Just ticketInfo -> do
                            let contextNFT = fst $ generateTicketACFromTicket (tiTsd ticketInfo)
                            liftIO clearScreen
                            txOutRef <- liftIO $ revealTicket (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust $ secretKey s) (Data.Text.unpack "marius") contextNFT Nothing
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (ctxProviders (providersCtx s))
                            continue
                              initialState
                                { message = "TICKET SECRET REVEALED SUCCESFULLY!\n" <> showText contextNFT <> "\n" <> showLink nid "tx" txOutRef <> "\n"
                                , currentScreen = MainScreen
                                }
                          Nothing -> continue s
                  _ -> do
                    mtForm1 <- handleFormEvent e (myTicketsForm s)
                    continue s {myTicketsForm = mtForm1}
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
          MyRafflesScreen ->
            let mrForm = myRafflesForm s
                mrFormState = formState mrForm
                msr = mrFormState ^. selectedMyRaffle
             in case key of
                  KChar 'c' -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        case msr of
                          Just sr -> do
                            let contextNFT = rRaffleID $ riRsd sr
                            liftIO clearScreen
                            txOutRef <- liftIO $ cancelRaffle (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust $ secretKey s) contextNFT Nothing
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (ctxProviders (providersCtx s))
                            continue
                              initialState
                                { message = "RAFFLE CANCELLED SUCCESFULLY!\n" <> showText contextNFT <> "\n" <> showLink nid "tx" txOutRef <> "\n"
                                , currentScreen = MainScreen
                                }
                          Nothing -> continue s
                  KChar 'r' -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        case msr of
                          Just sr -> do
                            let contextNFT = rRaffleID $ riRsd sr
                            liftIO clearScreen
                            txOutRef <- liftIO $ recoverStakeRaffle (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust $ secretKey s) contextNFT Nothing
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (ctxProviders (providersCtx s))
                            continue
                              initialState
                                { message = "RAFFLE STAKE RECOVERED SUCCESFULLY!\n" <> showText contextNFT <> "\n" <> showLink nid "tx" txOutRef <> "\n"
                                , currentScreen = MainScreen
                                }
                          Nothing -> continue s
                  KChar 'e' -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        case msr of
                          Just sr -> do
                            let contextNFT = rRaffleID $ riRsd sr
                            liftIO clearScreen
                            txOutRef <- liftIO $ recoverStakeAndAmountRaffle (RaffleizeOffchainContext validatorsTxOutRefs (providersCtx s)) (fromJust $ secretKey s) contextNFT Nothing
                            let nid = (cfgNetworkId . ctxCoreCfg . providersCtx) s
                            initialState <- liftIO $ buildInitialState (ctxProviders (providersCtx s))
                            continue
                              initialState
                                { message = "RAFFLE STAKE AND COLLECTED AMOUNT RECOVERED SUCCESFULLY!\n" <> showText contextNFT <> "\n" <> showLink nid "tx" txOutRef <> "\n"
                                , currentScreen = MainScreen
                                }
                          Nothing -> continue s
                  _ -> do
                    mrForm1 <- handleFormEvent e (myRafflesForm s)
                    continue s {myRafflesForm = mrForm1}
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
            let crForm = raffleConfigForm s
                invalid_fields = invalidFields crForm
             in case key of
                  KEnter -> case validatorsConfig s of
                    Nothing -> continue s {message = "Validators not present at " <> Data.Text.pack raffleizeValidatorsConfig}
                    Just validatorsTxOutRefs ->
                      do
                        let currentCVS = myConstrctValueState s
                        let stake = unFlattenValue $ toList $ listElements (constructedValueList currentCVS)
                        let mraffle = raffleFormToConfig (formState crForm) stake
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
                  KIns -> continue s {currentScreen = ConstructValueScreen}
                  _ -> do
                    crForm1 <- handleFormEvent e crForm
                    let crFormState = formState crForm1
                    let fieldValidations =
                          [ setFieldValid (isJust $ gyIso8601ParseM @Maybe (Data.Text.unpack (crFormState ^. commitDdl))) CommitDdlField
                          , setFieldValid (isJust $ gyIso8601ParseM @Maybe (Data.Text.unpack (crFormState ^. revealDdl))) RevealDdlField
                          , setFieldValid (crFormState ^. ticketPrice > (fromIntegral (rMinTicketPrice mockRaffleParam) `div` 1000000)) TicketPriceField
                          , setFieldValid (liftA2 (&&) (> 0) (< fromIntegral (rMaxNoOfTickets mockRaffleParam)) $ crFormState ^. minNoTickets) MinNoTokensField
                          , setFieldValid ((not . null) (toList (constructedValueList (myConstrctValueState s)))) ConstructedValueItemsList
                          ]
                    let validated_form = foldr' ($) crForm1 fieldValidations
                    continue s {raffleConfigForm = validated_form}
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
            (KChar c) -> case c of
              'R' -> continue s {message = "Refresh Screen"}
              'q' -> halt s
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
                        continue
                          s
                            { currentScreen = CreateRaffleScreen
                            }
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
        , if currentScreen s == ConstructValueScreen then drawConstructValueWidget (myConstrctValueState s) else emptyWidget
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
          , assetsWidget (cfgNetworkId ((ctxCoreCfg . providersCtx) s)) (secretKey s) (balanceList s)
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

assetsBalanceWidget :: (Traversable t, Splittable t) => GenericList NameResources t (CurrencySymbol, TokenName, Integer) -> Widget NameResources
assetsBalanceWidget valueItemsList =
  vLimit 100 $
    hLimit 150 $
      borderWithLabel (txt "ASSETS") $
        -- visible $
        --   withVScrollBarHandles $
        --     withVScrollBars OnRight $
        --       viewport ValueItemsViewPort Vertical $
        --         vLimit 300 $
        --           hLimit 150 $
        renderList (valueItemWidget True) False valueItemsList

stakeWidget :: (Traversable t, Splittable t) => GenericList NameResources t (CurrencySymbol, TokenName, Integer) -> Widget NameResources
stakeWidget stake =
  vLimit 100 $
    hLimit 150 $
      borderWithLabel (txt "ASSETS") $
        renderList (valueItemWidget True) False stake

addressWidget :: GYNetworkId -> GYAddress -> Widget n
addressWidget nid addr =
  let addrText = addressToText addr
   in hyperlink (showLink nid "address" addrText) $ withAttr "good" $ txt addrText

assetsWidget :: GYNetworkId -> Maybe GYPaymentSigningKey -> GenericList NameResources [] (CurrencySymbol, TokenName, Integer) -> Widget NameResources
assetsWidget nid (Just skey) valList =
  let addr = addressFromPaymentSigningKey nid skey
   in borderWithLabel (txt "WALLET") $
        hBox
          [ assetsAdaWidget nid addr (unFlattenValue $ listElements valList)
          , vBorder
          , assetsBalanceWidget valList
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
drawRaffleActionLabel ("User", "BuyTicket") = txt "[B]     - Buy a ticket for the selected raffle"
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
drawMyRaffleActionLabel ("RaffleOwner", "Cancel") = txt "[C]     - Cancel the selected raffle"
drawMyRaffleActionLabel ("RaffleOwner", "Update") = txt "[U]     - Update raffle configuration of the raffle"
drawMyRaffleActionLabel ("RaffleOwner", "RecoverStake") = txt "[R]     - Recover the stake of the selected raffle"
drawMyRaffleActionLabel ("RaffleOwner", "RecoverStakeAndAmount") = txt "[E]     - Recover the stake and collect the accumulated amount of the selected raffle"
drawMyRaffleActionLabel ("RaffleOwner", "CollectAmount") = txt "[A]     - Collect the accumulated amount of the selected raffle"
drawMyRaffleActionLabel _ = emptyWidget

------------------------------------------------------------------------------------------------

-- **  Create Raffle Screen

------------------------------------------------------------------------------------------------

createRaffleScreen :: RaffleizeUI -> Widget NameResources
createRaffleScreen s =
  let
    currentStakeValueList = toList (constructedValueList (myConstrctValueState s))
    currentStakeValue = unFlattenValue currentStakeValueList
    raffleConfigFormState = formState (raffleConfigForm s)
    ivfs = invalidFields (raffleConfigForm s)
    isValid = null ivfs
   in
    center $
      borderWithLabel (txt " CREATE NEW RAFFLE ") $
        vBox $
          padAll 1
            <$> [ withAttr focusedFormInputAttr (renderForm (raffleConfigForm s))
                , str (showValue "Current Raffle Stake" currentStakeValue)
                , if isValid then emptyWidget else hBorder <=> hCenter (invalidFieldsWidget ivfs) <=> hBorder
                , hCenter $ createRaffleActionsWidget isValid
                ]

createRaffleActionsWidget :: Bool -> Widget n
createRaffleActionsWidget isValid =
  withAttr "action" . borderWithLabel (txt "AVAILABLE ACTIONS") $
    vBox
      [ txt "[ESC]     - Close          "
      , txt "[Insert]  - Configure raffle value"
      , if isValid then txt ("[Enter]   - " <> "Create new raffle") else emptyWidget
      ]

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
            <$> [ withAttr focusedFormInputAttr (renderForm $ raffleizeForm s)
                , hBorder
                , hCenter $ invalidFieldsWidget ivfs
                , hBorder
                , hCenter $ formActionsWidget (null ivfs) actionsDesc
                ]

formActionsWidget :: Bool -> Text -> Widget n
formActionsWidget isValid desc =
  withAttr "action" . borderWithLabel (txt "AVAILABLE ACTIONS") $
    vBox
      [ txt "[ESC]   - Close          "
      , if isValid then txt ("[Enter] - " <> desc) else emptyWidget
      ]

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
