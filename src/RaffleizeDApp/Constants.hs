module RaffleizeDApp.Constants where

----------------
-- NFTs METADATA
----------------
metadataVersion :: Integer
metadataVersion = 1

raffleImageURI :: BuiltinString
raffleImageURI = "ipfs://bafybeiags6zsy42iplwkvj4m4crno57merd4a6uzkg62hdwli6qgl4n2ue"

raffleDescription :: BuiltinString
raffleDescription = "Ownership of a raffle"

raffleName :: BuiltinString
raffleName = "RAFFLEIZE: RAFFLE OWNER NFT"

ticketImageURI :: BuiltinString
ticketImageURI = "ipfs://bafybeid4u7fhlmw6mmha7hur7xojxqp26bxuxkpwke2a44qeeevnfvcgpy"

ticketDescription :: BuiltinString
ticketDescription = "Ownership of a raffle ticket"

ticketName :: BuiltinString
ticketName = "RAFFLEIZE: TICKET OWNER NFT"

--- Configuration FilePaths  ---

raffleizeLogoPath :: FilePath
raffleizeLogoPath = "raffleize.logo"

atlasCoreConfig :: FilePath
atlasCoreConfig = "atlas_config.json"

raffleizeValidatorsConfig :: FilePath
raffleizeValidatorsConfig = "raffleize_validators_config.json"

operationSkeyFilePath :: FilePath
operationSkeyFilePath = "operation.skey"

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