module RaffleizeDApp.Constants where

----------------
-- NFTs METADATA
----------------
metadataVersion :: Integer
metadataVersion = 1

raffleImageURI :: BuiltinString
raffleImageURI = "ipfs://QmTmZCrE6PCCn1cUrxjEzcZEE73PEUh7eqCCCPhs1KwMH6"

raffleDescription :: BuiltinString
raffleDescription = "This is a tokenized raffle"

raffleName :: BuiltinString
raffleName = "RAFFLEIZE: RAFFLE OWNER NFT"

ticketImageURI :: BuiltinString
ticketImageURI = "ipfs://QmYWtPVSDCfZp2KAtqZCfLP21WDSUrLB6rfmvByTcDqPJf"

ticketDescription :: BuiltinString
ticketDescription =  "This is a tokenized raffle ticket"

ticketName :: BuiltinString
ticketName = "RAFFLEIZE: TICKET OWNER NFT"



raffleizeLogoPath :: FilePath
raffleizeLogoPath = "raffleize.logo"

--- Configuration FilePaths  ---


atlasCoreConfig :: FilePath
atlasCoreConfig = "atlas_config.json"

operationSkeyFilePath :: FilePath
operationSkeyFilePath = "operation.skey"