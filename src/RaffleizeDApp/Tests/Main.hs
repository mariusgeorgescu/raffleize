module Main where

import PropertyTests (qcProps)
import Test.Tasty (defaultMain, testGroup)
import UnitTests (unitTests)

runTest :: IO ()
runTest =
  defaultMain (testGroup "Tests" [qcProps, unitTests])

main :: IO ()
main = do
  runTest