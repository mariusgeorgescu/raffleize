module RaffleizeDApp.Tests.PropertyTests where

import RaffleizeDApp.CustomTypes.ActionTypes (RaffleizeAction)
import RaffleizeDApp.OnChain.RaffleizeLogic (
  actionToLabel,
  validActionLabelsForRaffleState,
  validRaffleStatesForRaffleizeAction,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck qualified as QC

qcProps :: TestTree
qcProps =
  testGroup
    "(checked by QuickCheck)"
    [ stateToActionsConsistency
    ]

stateToActionsConsistency :: TestTree
stateToActionsConsistency =
  QC.testProperty "RaffleStateId - RaffleAction relation is consistent with RaffleStateId - RaffleActionLabel relation" $
    \(action :: RaffleizeAction) -> all (\state -> actionToLabel action `elem` validActionLabelsForRaffleState state) $ validRaffleStatesForRaffleizeAction action
