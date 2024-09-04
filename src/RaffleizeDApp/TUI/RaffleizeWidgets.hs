
module RaffleizeWidgets where

import Brick
import Brick.Focus
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Brick.Widgets.Table
import Control.Lens
import Data.Maybe qualified
import Data.Text qualified
import Data.Vector qualified
import GeniusYield.GYConfig
import GeniusYield.Types
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Time qualified
import PlutusLedgerApi.V1.Value
import PlutusPrelude (showText)
import PlutusTx.Show qualified
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.ActionTypes
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.CustomTypes.TicketTypes
import RaffleizeDApp.CustomTypes.TransferTypes

import RaffleizeDApp.OnChain.Utils
import RaffleizeDApp.TxBuilding.Utils
import RaffleizeDApp.TxBuilding.Validators
import Types

txOutRefWidget :: GYNetworkId -> GYTxOutRef -> Widget n
txOutRefWidget nid t =
  let txoutrefText = showTxOutRef t
   in hyperlink (showLink nid "tx" txoutrefText) $ withAttr (attrName "good") $ txt txoutrefText

printProvider :: GYCoreConfig -> Widget n
printProvider cfg = withAttr (attrName "good") . txt $ case cfgCoreProvider cfg of
  (GYNodeKupo {}) -> "KUPO"
  (GYMaestro {}) -> "MAESTRO"
  (GYBlockfrost {}) -> "BLOCKFROST"

printNetwork :: GYCoreConfig -> Widget n
printNetwork cfg = withAttr (attrName "good") . txt $ case cfgNetworkId cfg of
  GYMainnet -> "MAINNET"
  GYTestnetPreprod -> "PREPROD"
  GYTestnetPreview -> "PREVIEW"
  GYTestnetLegacy -> "TESTNET-LEGACY"
  GYPrivnet _f -> "PRIVNET"

symbolWidget :: Bool -> Widget n
symbolWidget v = if v then withAttr (attrName "good") $ txt "✔" else withAttr (attrName "warning") $ txt "X"

assetClassItemWidget :: CurrencySymbol -> TokenName -> Widget n
assetClassItemWidget cs tn = str (show cs) <=> hBorder <=> str (toString tn)

valueItemWidget :: Bool -> Bool -> (CurrencySymbol, TokenName, Integer) -> Widget n
valueItemWidget selectable hasFocus (cs, tn, i) =
  (if hasFocus && selectable then withAttr (attrName "selected") else id) $
    vLimit 5 $
      hLimit 130 $
        border
          ( assetClassItemWidget cs tn
              <+> vBorder
              <+> center (str (show i))
          )

drawValueWidget :: Value -> Widget NameResources
drawValueWidget val =
  vBox $
    valueItemWidget False False <$> flattenValue val

drawPOSIX :: PlutusLedgerApi.V1.Time.POSIXTime -> Widget NameResources
drawPOSIX = str . gyIso8601Show . timeFromPlutus

------------------------------------------------------------------------------------------------

-- **  Form Screens

------------------------------------------------------------------------------------------------

-- Common Helper Functions

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
  ConstructedValueItemsList -> "Stake value must not be empty ! The stake value can contain maximum 2 asset classes"
  RevealedSecretField -> "The secret must hash must match with the onchain secret hash!"
  SecretField -> "The secret must have maximum 32 characters !"
  MnemonicField -> "Enter a valid recovery phrase !"
  _ -> ""

invalidFieldsWidget :: [NameResources] -> Widget n
invalidFieldsWidget ivfs = withAttr (attrName "warning") $ txt (Data.Text.intercalate "\n" $ invalidFieldText <$> ivfs)

mkFormScreen :: Text -> Text -> Form s e NameResources -> Widget NameResources
mkFormScreen title actionsDesc raffleizeForm =
  let ivfs = invalidFields raffleizeForm
   in borderWithLabel (txt title) $
        vBox $
          padAll 1
            <$> [ withAttr focusedFormInputAttr (renderForm raffleizeForm)
                , hBorder
                , hCenter $ invalidFieldsWidget ivfs
                , hBorder
                , hCenter $ formActionsWidget (null ivfs) actionsDesc []
                ]

formActionsWidget :: Bool -> Text -> [Widget n] -> Widget n
formActionsWidget isValid desc other =
  withAttr (attrName "action") . borderWithLabel (txt "AVAILABLE ACTIONS") $
    vBox
      ( [ txt "[ESC]   - Close          "
        , if isValid then txt ("[Enter] - " <> desc) else emptyWidget
        ]
          <> other
      )

------------------------------------------------------------------------------------------------

-- *  Import Wallet Form

------------------------------------------------------------------------------------------------

newtype MnemonicFormState = MnemonicFormState
  { _mnemonicField :: Text
  }
  deriving (Show)

makeLenses ''MnemonicFormState

mkMnemonicForm :: MnemonicFormState -> Form MnemonicFormState e NameResources
mkMnemonicForm =
  newForm
    [ (txt "Recovery Phrase: " <+>) @@= editTextField mnemonicField MnemonicField (Just 5)
    ]

drawMnemonicForm :: Form s e NameResources -> Widget NameResources
drawMnemonicForm = center . mkFormScreen " WALLET FROM MNEMONIC " " IMPORT FROM RECOVERY PHRASE "

------------------------------------------------------------------------------------------------

-- *  Mint test tokens

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

drawMintTestTokensForm :: Form s e NameResources -> Widget NameResources
drawMintTestTokensForm = center . mkFormScreen " MINT TEST TOKENS " " MINT TEST TOKENS "

------------------------------------------------------------------------------------------------

-- * View all active raffles

------------------------------------------------------------------------------------------------
data ActiveRafflesFormState = ActiveRafflesFormState
  { _activeRaffles :: Data.Vector.Vector RaffleInfo
  , _selectedRaffle :: Maybe RaffleInfo
  }
  deriving (Show)

makeLenses ''ActiveRafflesFormState

drawRaffleLockedValue :: Value -> Widget NameResources
drawRaffleLockedValue riValue = borderWithLabel (txt " CURRENT LOCKED VALUE ") $ vLimit 200 $ hLimit 60 $ drawValueWidget riValue

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
drawRaffleInfoListItem isSelected ri = if isSelected then withAttr (attrName "action") $ drawRaffleInfo ri else drawRaffleInfo ri

mkActiveRafflesForm :: ActiveRafflesFormState -> Form ActiveRafflesFormState e NameResources
mkActiveRafflesForm =
  newForm
    [(txt mempty <=>) @@= listField _activeRaffles selectedRaffle drawRaffleInfoListItem 10 ActiveRafflesListField]

------------------------------------------------------------------------------------------------

-- **  Active Raffles Screen

------------------------------------------------------------------------------------------------

drawActiveRafflesForm :: Form ActiveRafflesFormState e NameResources -> Widget NameResources
drawActiveRafflesForm arForm =
  let
    arFormState = formState arForm
    currentActions = riAvailableActions <$> (arFormState ^. selectedRaffle)
   in
    center $
      borderWithLabel (txt " ACTIVE RAFFLES ") $
        vBox $
          padAll 1
            <$> [ center $ renderForm arForm
                , hBorder
                , hCenter $ drawRaffleActionsWidget currentActions
                ]
  where
    drawRaffleActionsWidget :: Maybe [RaffleizeActionLabel] -> Widget NameResources
    drawRaffleActionsWidget mActions =
      withAttr (attrName "action") $
        borderWithLabel (txt "AVAILABLE ACTIONS") $
          vBox $
            maybe [] (drawRaffleActionLabel <$>) mActions
              ++ [txt "[ESC]   - Close          "]

    drawRaffleActionLabel :: RaffleizeActionLabel -> Widget NameResources
    drawRaffleActionLabel ("User", "BuyTicket") = txt "[B]     - Buy a ticket for the selected raffle"
    drawRaffleActionLabel _ = emptyWidget

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

-- **  My Raffles Screen

------------------------------------------------------------------------------------------------

drawMyRafflesForm :: Form MyRafflesFormState RaffleizeEvent NameResources -> Widget NameResources
drawMyRafflesForm mrForm =
  let
    mrFormState = formState mrForm
    currentActions = riAvailableActions <$> (mrFormState ^. selectedMyRaffle)
   in
    center $
      borderWithLabel (txt " MY RAFFLES ") $
        vBox $
          padAll 1
            <$> [ center $ renderForm mrForm
                , hBorder
                , hCenter $ drawMyRaffleActionsWidget currentActions
                ]
  where
    drawMyRaffleActionsWidget :: Maybe [RaffleizeActionLabel] -> Widget NameResources
    drawMyRaffleActionsWidget mActions =
      withAttr (attrName "action") $
        borderWithLabel (txt "AVAILABLE ACTIONS") $
          vBox $
            maybe [] (drawMyRaffleActionLabel <$>) mActions
              ++ [txt "[ESC]   - Close          "]
      where
        drawMyRaffleActionLabel :: RaffleizeActionLabel -> Widget NameResources
        drawMyRaffleActionLabel ("RaffleOwner", "Cancel") = txt "[C]     - Cancel the selected raffle"
        drawMyRaffleActionLabel ("RaffleOwner", "Update") = txt "[U]     - Update raffle configuration of the raffle"
        drawMyRaffleActionLabel ("RaffleOwner", "RecoverStake") = txt "[R]     - Recover the stake of the selected raffle"
        drawMyRaffleActionLabel ("RaffleOwner", "RecoverStakeAndAmount") = txt "[E]     - Recover the stake and collect the accumulated amount of the selected raffle"
        drawMyRaffleActionLabel ("RaffleOwner", "CollectAmount") = txt "[A]     - Collect the accumulated amount of the selected raffle"
        drawMyRaffleActionLabel _ = emptyWidget

------------------------------------------------------------------------------------------------

-- **  My Tickets Form

------------------------------------------------------------------------------------------------
data MyTicketsFormState = MyTicketsFormState
  { _myTickets :: Data.Vector.Vector TicketInfo
  , _selectedTicket :: Maybe TicketInfo
  }
  deriving (Show)

makeLenses ''MyTicketsFormState

drawTicketInfo :: TicketInfo -> Widget NameResources
drawTicketInfo TicketInfo {..} =
  joinBorders
    <$> borderWithLabel
      (txt (showText (tRaffle tiTsd)))
    $ hBox
      ( joinBorders
          <$> [ drawTicketState
              , drawTicketLockedValue
              ]
      )
  where
    drawTicketLockedValue :: Widget NameResources
    drawTicketLockedValue = borderWithLabel (txt " CURRENT LOCKED VALUE ") $ drawValueWidget tiValue
    drawTicketState :: Widget NameResources
    drawTicketState =
      vLimit 200 $
        hLimit 130 $
          renderTable $
            table
              [ [txt "Ticket Number: ", txt (showText (tNumber tiTsd))]
              , [txt "Ticket State: ", txt (showText tiStateLabel)]
              , [txt "Secret Hash: ", txt (fromBuiltin @BuiltinString $ PlutusTx.Show.show (tSecretHash tiTsd))]
              , [txt "Revealed Secret: ", txt (showText (tSecret tiTsd))]
              ]

mkMyTicketsForm :: MyTicketsFormState -> Form MyTicketsFormState e NameResources
mkMyTicketsForm =
  newForm
    [(txt "My Tickets" <=>) @@= listField _myTickets selectedTicket drawTicketInfoListItem 10 MyTicketsListField]
  where
    drawTicketInfoListItem :: Bool -> TicketInfo -> Widget NameResources
    drawTicketInfoListItem isSelected ti = if isSelected then withAttr (attrName "action") $ drawTicketInfo ti else drawTicketInfo ti

------------------------------------------------------------------------------------------------

-- **  My Tickets Screen

------------------------------------------------------------------------------------------------

drawMyTicketsScreen :: Form MyTicketsFormState RaffleizeEvent NameResources -> Widget NameResources
drawMyTicketsScreen mtForm =
  let
    mtFormState = formState mtForm
    currentActions = tiAvailableActions <$> (mtFormState ^. selectedTicket)
   in
    center $
      borderWithLabel (txt " MY TICKETS ") $
        vBox $
          padAll 1
            <$> [ center $ renderForm mtForm
                , hBorder
                , hCenter $ drawMyTicketsActionsWidget currentActions
                ]
  where
    drawMyTicketsActionsWidget :: Maybe [RaffleizeActionLabel] -> Widget NameResources
    drawMyTicketsActionsWidget mActions =
      withAttr (attrName "action") $
        borderWithLabel (txt "AVAILABLE ACTIONS") $
          vBox $
            maybe [] (drawMyTicketActionLabel <$>) mActions
              ++ [txt "[ESC]   - Close          "]
      where
        drawMyTicketActionLabel :: RaffleizeActionLabel -> Widget NameResources
        drawMyTicketActionLabel label = case label of
          ("TicketOwner", "RevealTicketSecret") -> txt "[S]     - Reveal the ticket secret"
          ("TicketOwner", "CollectStake") -> txt "[W]     - Redeem the winning ticket"
          ("TicketOwner", "RefundTicket") -> txt "[R]     - Get full refund"
          ("TicketOwner", "RefundTicketExtra") -> txt "[E]     - Get ticket refund and extra"
          ("TicketOwner", "RefundCollateralLosing") -> txt "[L]     - Get refund of the collateral on losing ticket"
          ("RaffleOwner", "GetCollateraOfExpiredTicket") -> emptyWidget
          _ -> emptyWidget

------------------------------------------------------------------------------------------------

-- **  Reveal Ticket Secret Form

------------------------------------------------------------------------------------------------
newtype RevealSecretFormState = RevealSecretFormState
  { _revealedSecret :: Text
  }
  deriving (Show)

makeLenses ''RevealSecretFormState

mkRevealSecretForm :: RevealSecretFormState -> Form RevealSecretFormState e NameResources
mkRevealSecretForm =
  newForm
    [(txt "Ticket Secret: " <=>) @@= editTextField revealedSecret RevealedSecretField (Just 1)]

drawRevealTicketSecretScreen :: Maybe (AssetClass, Integer) -> Form s e NameResources -> Widget NameResources
drawRevealTicketSecretScreen (Just (selectedTicketRaffleId, selectedTicketNo)) revealSecretForm = center $ mkFormScreen (" REVEAL SECRET FOR TICKET  #" <> showText selectedTicketNo <> " OF RAFFLE " <> showText selectedTicketRaffleId) " REVEAL SECRET " revealSecretForm
drawRevealTicketSecretScreen Nothing _ = error "Trying to reveal ticket with no selected ticket !"

------------------------------------------------------------------------------------------------

-- **  Add To Stake Form

------------------------------------------------------------------------------------------------

newtype AddToValueFormState = AddToValueFormState
  { _amount :: Int
  }
  deriving (Show)

makeLenses ''AddToValueFormState

mkAddToStakeFormState :: AddToValueFormState -> Form AddToValueFormState e NameResources
mkAddToStakeFormState =
  newForm
    [(txt "Amount: " <=>) @@= editShowableField amount AddToValueAmountField]

------------------------------------------------------------------------------------------------

-- ** Construct Value Form

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
                  , (if currentFocus /= 3 then id else withAttr focusedFormInputAttr) (txt "Current Selected Value " <=> renderList (valueItemWidget True) (currentFocus == 2) constructedValueList)
                  ]
          )
          <=> vBox
            [ if isValidToAdd then emptyWidget else hBorder <=> hCenter (invalidFieldsWidget ivfs) <=> hBorder
            , constructValueActionsWidget isValidToAdd (null constructedValueList) currentFocus
            ]

constructValueActionsWidget :: Bool -> Bool -> Integer -> Widget n
constructValueActionsWidget isValidToAdd isEmptyStake currentFocus =
  withAttr (attrName "action") . borderWithLabel (txt "AVAILABLE ACTIONS") $
    vBox
      [ txt "[ESC]           - Close          "
      , if currentFocus `elem` [1, 2] then (if isValidToAdd then txt "[Insert] | [+]  - Add to value" else emptyWidget) else (if isEmptyStake then emptyWidget else txt "[Delete] | [-]  - Remove from value")
      , if not isEmptyStake then txt "[Enter]         - Finish value construction" else emptyWidget
      ]

drawAction :: (Text, Text) -> Widget n
drawAction (bkey, bdesc) = txt $ "[ " <> bkey <> " ]" <> "  -  " <> bdesc

drawAvailableActions :: [Maybe (Text, Text)] -> Widget n
drawAvailableActions mactions =
  withAttr (attrName "action") . borderWithLabel (txt "AVAILABLE ACTIONS") $
    vBox $
      drawAction <$> Data.Maybe.catMaybes mactions

------------------------------------------------------------------------------------------------

-- **  Create Raffle Screen

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
    , (txt "Recipient address: " <+>) @@= editShowableFieldWithValidate raffleRecipient SendRaffleAddressField (liftA2 (||) (Data.Maybe.isJust . addressFromTextMaybe) Data.Text.null)
    ]

drawCreateUpdateRaffleForm :: Maybe AssetClass -> Form RaffleConfigFormState RaffleizeEvent NameResources -> ConstructValueState -> Widget NameResources
drawCreateUpdateRaffleForm updateRaffle raffleConfigForm constructValueState =
  let
    (title, actionDesc :: Text) = case updateRaffle of
      Nothing -> (" CREATE NEW RAFFLE ", "Create new raffle")
      Just rid -> (" UPDATE RAFFLE " <> showText rid, "Update raffle configuration")
    currentStakeValueList = toList (constructedValueList constructValueState)
    currentStakeValue = unFlattenValue currentStakeValueList
    raffleConfigFormState = formState raffleConfigForm
    ivfs = invalidFields raffleConfigForm <> ([ConstructedValueItemsList | (not . (`elem` [1, 2]) . length) (toList (constructedValueList constructValueState))])
    isValid = null ivfs
   in
    center $
      borderWithLabel (txt title) $
        vBox $
          padAll 1
            <$> [ withAttr focusedFormInputAttr (renderForm raffleConfigForm)
                , str (showValue "Current Raffle Stake" currentStakeValue)
                , if isValid then emptyWidget else hBorder <=> hCenter (invalidFieldsWidget ivfs) <=> hBorder
                , hCenter $ formActionsWidget isValid actionDesc [txt "[Insert] | [+] - Configure raffle value"]
                ]

drawCreateRaffleForm :: Form RaffleConfigFormState RaffleizeEvent NameResources -> ConstructValueState -> Widget NameResources
drawCreateRaffleForm = drawCreateUpdateRaffleForm Nothing

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
    [ (txt "Ticket Secret: " <=>) @@= editShowableFieldWithValidate secret SecretField ((<= secretMaxLength) . fromIntegral . Data.Text.length)
    , (txt "Recipient address: " <=>) @@= editShowableFieldWithValidate ticketRecipient SendTicketAddressField (liftA2 (||) (Data.Maybe.isJust . addressFromTextMaybe) Data.Text.null)
    ]

drawBuyTicketForm :: Maybe AssetClass -> Form BuyTicketFormState e NameResources -> Widget NameResources
drawBuyTicketForm (Just selectedRaffleID) buyTicketForm = center $ mkFormScreen (" BUY TICKET FOR " <> showText selectedRaffleID) " BUY TICKET " buyTicketForm
drawBuyTicketForm Nothing _ = error "Trying to buy ticket with no selected raffle !"

--------------------------