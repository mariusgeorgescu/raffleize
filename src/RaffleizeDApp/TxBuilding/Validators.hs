module RaffleizeDApp.TxBuilding.Validators where

import GeniusYield.Types
import PlutusLedgerApi.V3
import PlutusTx
import RaffleizeDApp.Constants
import RaffleizeDApp.CustomTypes.RaffleTypes
import RaffleizeDApp.OnChain.RaffleizeMintingPolicy
import RaffleizeDApp.OnChain.RaffleizeTicketValidator
import RaffleizeDApp.OnChain.RaffleizeValidator

------------------------------------------------------------------------------------------------

-- *  Define Validator

------------------------------------------------------------------------------------------------

adminAddress :: PubKeyHash
adminAddress = "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"


raffleizeValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
raffleizeValidatorPlutus = compileRaffleizeValidator adminAddress

raffleizeValidatorGY :: GYScript 'PlutusV3
raffleizeValidatorGY = validatorFromPlutus raffleizeValidatorPlutus

raffleizeValidatorHashGY :: GYScriptHash
raffleizeValidatorHashGY = validatorHash raffleizeValidatorGY

raffleizeValidatorHashPlutus :: ScriptHash
raffleizeValidatorHashPlutus = validatorHashToPlutus raffleizeValidatorHashGY

------------------------------------------------------------------------------------------------

-- *  Define Ticket Validator

------------------------------------------------------------------------------------------------


ticketValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
ticketValidatorPlutus = compileTicketValidator adminAddress

ticketValidatorGY :: GYScript 'PlutusV3
ticketValidatorGY = validatorFromPlutus ticketValidatorPlutus

ticketValidatorHashGY :: GYScriptHash
ticketValidatorHashGY = validatorHash ticketValidatorGY

ticketValidatorHashPlutus :: ScriptHash
ticketValidatorHashPlutus = validatorHashToPlutus ticketValidatorHashGY

------------------------------------------------------------------------------------------------

-- *  Define Minting Policy

------------------------------------------------------------------------------------------------

mockRaffleParam :: RaffleParam
mockRaffleParam =
  RaffleParam
    { rMaxNoOfTickets = 20
    , rMinRevealingWindow = 6_000 --- ^ Miliseconds
    , rMinTicketPrice = 3_000_000 --- ^ Lovelaces
    , rRaffleValidatorHash = raffleizeValidatorHashPlutus
    , rTicketValidatorHash = ticketValidatorHashPlutus
    , rTicketCollateral = 3_500_000 --- ^ Lovelaces
    , rRaffleCollateral = 30_000_000 --- ^ Lovelaces
    }


raffleizeMintingPolicyPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
raffleizeMintingPolicyPlutus = compileRaffleizeMP mockRaffleParam

raffleizeMintingPolicyGY :: GYScript 'PlutusV3
raffleizeMintingPolicyGY = mintingPolicyFromPlutus raffleizeMintingPolicyPlutus

exportRaffleScript :: IO ()
exportRaffleScript = writeScript @'PlutusV3 raffleizeValidatorFile $ validatorToScript raffleizeValidatorGY

exportTicketScript :: IO ()
exportTicketScript = writeScript @'PlutusV3 ticketValidatorFile $ validatorToScript ticketValidatorGY

exportMintingPolicy :: IO ()
exportMintingPolicy = writeScript @'PlutusV3 mintingPolicyFile $ mintingPolicyToScript raffleizeMintingPolicyGY
