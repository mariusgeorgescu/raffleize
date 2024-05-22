module RaffleizeDApp.Constants where

----------------
-- NFTs METADATA
----------------
metadataVersion :: Integer
metadataVersion = 1

raffleImageURI :: BuiltinString
raffleImageURI = "http://ipfs.io/ipfs/QmTmZCrE6PCCn1cUrxjEzcZEE73PEUh7eqCCCPhs1KwMH6" --"ipfs://QmTmZCrE6PCCn1cUrxjEzcZEE73PEUh7eqCCCPhs1KwMH6"

raffleDescription :: BuiltinString
raffleDescription = "This is a tokenized raffle"

raffleName :: BuiltinString
raffleName = "RAFFLEIZE: RAFFLE OWNER NFT"

ticketImageURI :: BuiltinString
ticketImageURI = "ipfs://QmYWtPVSDCfZp2KAtqZCfLP21WDSUrLB6rfmvByTcDqPJf"

ticketDescription :: BuiltinString
ticketDescription = "This is a tokenized raffle ticket"

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