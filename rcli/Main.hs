module Main where
import Data.ByteString.Lazy qualified as B
import RaffleizeDApp.CustomTypes.RaffleTypes (RaffleConfig)
import Data.Aeson 

jsonFile :: FilePath
jsonFile = "raffleconfig.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO () -- TODO (develop cli)
main = do
  bs <- getJSON
  let config = decode @RaffleConfig bs
  print $  fromJust config 