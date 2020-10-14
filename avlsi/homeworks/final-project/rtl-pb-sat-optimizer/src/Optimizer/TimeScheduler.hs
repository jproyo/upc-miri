{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      : Optimizer.TimeScheduler
-- Description : This module contains the encoding
-- Copyright   : (c) Juan Pablo Royo Sales, 2020
-- License     : GPL-3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : educational
-- Portability : POSIX
--
-- Main entry point of the optimizer for Time-Constrained Schedule and Resource-Constrained Schedule
module Optimizer.TimeScheduler where

import Control.Lens
import qualified Data.PseudoBoolean as PB
import Data.Schedule
import Data.Set.Lens
import Data.List (groupBy)
import Relude

encodeObjectiveFunction :: MonadState Int m => Schedule -> m PB.Sum
encodeObjectiveFunction sc = sc ^. sResources . to (runReader resources') & traverseOf each convertToSum
  where
    convertToSum (i, _) = (i,) . flip (:) [] <$> (modify (+ 1) >> get)

encodeUniqueConstraints :: MonadState Int m => Schedule -> m [PB.Constraint]
encodeUniqueConstraints = foldMapM (updateState . nodeUnique') . combinedList

encodePrecedenceConstraints :: MonadState Int m => Schedule -> m [PB.Constraint]
encodePrecedenceConstraints sc = do
  let maps = runReader toMapNodes sc
  foldMapM (fmap (filter (not . null . view _1)) . updateState . precedence maps) . combinedList $ sc

encodeResourceConstraints :: MonadState Int m => Schedule -> m [PB.Constraint]
encodeResourceConstraints = 
  foldMapM resourceConstraint . combinedListByStep

combinedListByStep :: Schedule -> Map Int [Map Resource [Int]]
combinedListByStep = error "not implemented"

resourceConstraint :: [Map Resource [Int]] -> m [PB.Constraint]
resourceConstraint = error "not implemented"

combinedList :: Schedule -> [(Node, Node)]
combinedList sc = zip (sc ^. sAsap) (sc ^. sAlap)

toMapNodes :: Reader Schedule (Map Int Node, Map Int Node)
toMapNodes = do
  asap <- view sAsap
  alap <- view sAlap
  return (toMap asap, toMap alap)

toMap :: [Node] -> Map Int Node
toMap = fromList . fmap (\x -> (x ^. nId, x))

precedence :: (Map Int Node, Map Int Node) -> (Node, Node) -> (Int, [PB.Constraint])
precedence (asap, alap) (n1, n2) =
  let nodeId1 = n1 ^. nId
      nStartStep1 = n1 ^. nStartStep
      nEndStep2 = n2 ^. nEndStep
      toNode1 = toNode n1 asap
      toNode2 = toNode n1 alap
      toNode node schedule = node ^? nToNode . _Just . to (flip view schedule . at) . folded
      bList [tN1, tN2] =
        [ [ (fromIntegral $ negate s, [(nodeId1 * 10) + s]),
            (fromIntegral sTn, [(tN1 ^. nId * 10) + sTn])
          ]
          | s <- [nStartStep1 .. nEndStep2],
            sTn <- [(tN1 ^. nStartStep) .. (tN2 ^. nEndStep)]
        ]
      bList _ = []
      list = mconcat . bList . catMaybes $ [toNode1, toNode2]
   in (calculateMaxX list, [(toList $ setOf folded list, PB.Ge, 1)])

nodeUnique' :: (Node, Node) -> (Int, [PB.Constraint])
nodeUnique' (n1, n2) =
  let nId1 = n1 ^. nId
      nStart1 = n1 ^. nStartStep
      nEnd2 = n2 ^. nEndStep
      list = [(1, [(nId1 * 10) + s]) | s <- [nStart1 .. nEnd2]]
   in (calculateMaxX list, [(toList $ setOf folded list, PB.Eq, 1)])

updateState :: MonadState Int m => (Int, [PB.Constraint]) -> m [PB.Constraint]
updateState fn = do
  let (x', constraints) = fn
  modify (max x')
  return constraints

calculateMaxX :: [PB.WeightedTerm] -> Int
calculateMaxX list = let maxL = (maximumOf traverse list ^.. folded . _2 . folded) & firstOf traverse
                      in maybe minInt identity maxL


resources' :: Reader ResourceList [(Integer, Resource)]
resources' =
  magnify (_Wrapped' . folded) $ do
    weight <- view rcWeight
    amount <- view rcAmount
    rType <- view rcResource
    return $ foldl' (createNode weight rType) [] [0 .. amount -1]

createNode :: Int -> Resource -> [(Integer, Resource)] -> Int -> [(Integer, Resource)]
createNode weight rType pbsum b =
  (fromIntegral ((2 ^ b) * weight), rType) : pbsum
