module RaffleizeDApp.TxBuilding.Validators where

import GeniusYield.Types
import PlutusLedgerApi.V2
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

raffleizeValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
raffleizeValidatorPlutus = compileRaffleizeValidator adminAddress

raffleizeValidatorGY :: GYValidator 'PlutusV2
raffleizeValidatorGY = validatorFromPlutus raffleizeValidatorPlutus

raffleizeValidatorHashGY :: GYValidatorHash
raffleizeValidatorHashGY = validatorHash raffleizeValidatorGY

raffleizeValidatorHashPlutus :: ScriptHash
raffleizeValidatorHashPlutus = validatorHashToPlutus raffleizeValidatorHashGY

------------------------------------------------------------------------------------------------

-- *  Define Ticket Validator

------------------------------------------------------------------------------------------------

ticketValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
ticketValidatorPlutus = compileTicketValidator adminAddress

ticketValidatorGY :: GYValidator 'PlutusV2
ticketValidatorGY = validatorFromPlutus ticketValidatorPlutus

ticketValidatorHashGY :: GYValidatorHash
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
    , rTicketCollateral = 3_000_000 --- ^ Lovelaces
    , rRaffleCollateral = 3_000_000 --- ^ Lovelaces
    }

raffleizeMintingPolicyPlutus :: CompiledCode (BuiltinData -> BuiltinData -> ())
raffleizeMintingPolicyPlutus = compileRaffleizeMP mockRaffleParam

raffleizeMintingPolicyGY :: GYMintingPolicy 'PlutusV2
raffleizeMintingPolicyGY = mintingPolicyFromPlutus raffleizeMintingPolicyPlutus

exportRaffleScript :: IO ()
exportRaffleScript = writeScript @'PlutusV2 raffleizeValidatorFile $ validatorToScript raffleizeValidatorGY

exportTicketScript :: IO ()
exportTicketScript = writeScript @'PlutusV2 ticketValidatorFile $ validatorToScript ticketValidatorGY

exportMintingPolicy :: IO ()
exportMintingPolicy = writeScript @'PlutusV2 mintingPolicyFile $ mintingPolicyToScript raffleizeMintingPolicyGY
