module RaffleizeDApp.Utils where

import Data.List.Extra (replicate)

greenColorString :: String -> String
greenColorString s =
  "\n"
    ++ "\ESC[1;32m"
    ++ replicate 100 '='
    ++ "\n"
    ++ s
    ++ "\n"
    ++ replicate 100 '='
    ++ "\ESC[0m"
    ++ "\n"

yellowColorString :: String -> String
yellowColorString s =
  "\n"
    ++ "\ESC[1;93m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"

blueColorString :: String -> String
blueColorString s =
  "\n"
    ++ "\ESC[1;94m"
    ++ s
    ++ "\ESC[0m"
    ++ "\n"
