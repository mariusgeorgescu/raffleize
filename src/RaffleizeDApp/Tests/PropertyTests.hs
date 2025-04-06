module PropertyTests where

import Test.Tasty (TestTree, testGroup)

-- import Test.Tasty.QuickCheck qualified as QC

qcProps :: TestTree
qcProps =
  testGroup
    "(checked by QuickCheck)"
    []
