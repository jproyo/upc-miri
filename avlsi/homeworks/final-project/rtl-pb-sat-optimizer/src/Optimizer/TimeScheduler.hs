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
import Relude

encodeObjectiveFunction :: MonadState Int m => Schedule -> m PB.Sum
encodeObjectiveFunction sc = sc ^. sResources . to (runReader resources') & traverseOf each convertToSum
  where
    convertToSum (i, _) = (i,) . flip (:) [] <$> (modify (+ 1) >> get)

encodeUniqueConstraints :: Schedule -> [PB.Constraint]
encodeUniqueConstraints sc = foldMap nodeUnique $ zip (sc ^. sAsap) (sc ^. sAlap)

encodePrecedenceConstraints :: Schedule -> PB.Sum
encodePrecedenceConstraints sc = foldMapOf (sAsap . folded) (precedence $ sc ^. sAsap) sc

precedence :: [Node] -> Node -> PB.Sum
precedence nodes n
  | n ^. nToNode . to isJust =
    let nodeId1 = n ^. nId
        toNode = find (\x -> maybe False ((x ^. nId) ==) (n ^. nToNode)) nodes
        nStartStep1 = n ^. nStartStep
        bList tN = [(fromIntegral nStartStep1, [(nodeId1 * 10) + nStartStep1]), (fromIntegral $ negate (tN ^. nStartStep), [(tN ^. nId * 10) + tN ^. nStartStep])]
     in maybe [] bList toNode
  | otherwise = []

nodeUnique :: (Node, Node) -> [PB.Constraint]
nodeUnique (n1, n2) =
  let nId1 = n1 ^. nId
      nStart1 = n1 ^. nStartStep
      nEnd2 = n2 ^. nEndStep
      list = [(1, [(nId1 * 10) + s]) | s <- [nStart1 .. nEnd2]]
   in [(toList $ setOf folded list, PB.Eq, 1)]

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
