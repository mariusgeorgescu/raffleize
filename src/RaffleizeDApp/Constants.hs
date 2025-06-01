module RaffleizeDApp.Constants where

import PlutusLedgerApi.Data.V3 (PubKeyHash)

adminPubKeyHash :: PubKeyHash
adminPubKeyHash = "7ee37a8916c68d31a62e347c7512d922ce999025c50b644ca2f6426b"



----------------
-- NFTs METADATA
----------------
metadataVersion :: Integer
metadataVersion = 1

raffleImageURI :: BuiltinString
raffleImageURI = "ipfs://bafybeiags6zsy42iplwkvj4m4crno57merd4a6uzkg62hdwli6qgl4n2ue"

raffleDescription :: BuiltinString
raffleDescription = "Raffle ownership"

raffleName :: BuiltinString
raffleName = "RAFFLEIZE: RAFFLE OWNER NFT"

ticketImageURI :: BuiltinString
ticketImageURI = "ipfs://bafkreiegckq33xoke7n7jqfc5jf23wyt2ccklz5itsxau2sttb63objiym"

ticketDescription :: BuiltinString
ticketDescription = "Ticket ownership"

ticketName :: BuiltinString
ticketName = "RAFFLEIZE: TICKET OWNER NFT"

--- Configuration FilePaths  ---

raffleizeLogoPath :: FilePath
raffleizeLogoPath = "raffleize.logo"

atlasCoreConfig :: FilePath
atlasCoreConfig = "atlas_config.json"

raffleizeValidatorsConfig :: FilePath
raffleizeValidatorsConfig = "raffleize_validators_config.json"

operationPrivFilePath :: FilePath
operationPrivFilePath = "operation.prv"

-- Exports

raffleizeValidatorFile :: FilePath
raffleizeValidatorFile = "raffleizeValidator.plutus"

ticketValidatorFile :: FilePath
ticketValidatorFile = "ticketValidator.plutus"

mintingPolicyFile :: FilePath
mintingPolicyFile = "raffleizeMintingPolicy.plutus"

--- Links

cexplorerPreview :: Text
cexplorerPreview = "https://preview.cexplorer.io/"

cexplorerPreprod :: Text
cexplorerPreprod = "https://preprod.cexplorer.io/"

cexplorerMainnet :: Text
cexplorerMainnet = "https://cexplorer.io/"

----------

tokenNameMaxLength :: Int
tokenNameMaxLength = 32

secretMaxLength :: Integer
secretMaxLength = 64

purescriptProjectSrcPath :: FilePath
purescriptProjectSrcPath = "../raffleize-frontend-purescript/src/"