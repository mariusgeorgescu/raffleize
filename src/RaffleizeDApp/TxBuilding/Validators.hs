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

raffleizeValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
raffleizeValidatorPlutus = compileRaffleizeValidator adminPubKeyHash

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
ticketValidatorPlutus = compileTicketValidator

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
    { rMaxNoOfTickets = 20,
      rMinRevealingWindow = 6_000, --- ^ Miliseconds
      rMinNotClosingWindow = 2_592_000_000, --- ^ Miliseconds ± 30 days
      rMinTicketPrice = 3_000_000, --- ^ Lovelaces
      rRaffleValidatorHash = raffleizeValidatorHashPlutus,
      rTicketValidatorHash = ticketValidatorHashPlutus,
      rTicketCollateral = 3_500_000, --- ^ Lovelaces
      rRaffleCollateral = 30_000_000 --- ^ Lovelaces
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
