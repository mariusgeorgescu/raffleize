module Main where

import RaffleizeDApp.Tests.PropertyTests (qcProps)
import RaffleizeDApp.Tests.UnitTests (unitTests)
import Test.Tasty (defaultMain, testGroup)

runTest :: IO ()
runTest =
  defaultMain (testGroup "Tests" [qcProps, unitTests])

main :: IO ()
main = do
  runTest