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
import qualified Data.Map as M
import Relude

data EncodedState = EncodedState
  { _esLastLit :: Int
  , _esResourceSlot :: Map Resource [Int]
  } deriving (Show, Eq)

makeLenses ''EncodedState

encodeObjectiveFunction :: MonadState EncodedState m => Schedule -> m PB.Sum
encodeObjectiveFunction sc = sc ^. sResources . to (runReader resources') & traverseOf each convertToSum
  where
    convertToSum (i, r) = (i,) . flip (:) [] . flip (^.) esLastLit <$> (modify (execState (updateResourceEncodedState r)) >> get)

updateResourceEncodedState :: Resource -> State EncodedState ()
updateResourceEncodedState r = do 
  esLastLit += 1 
  lastLit <- use esLastLit
  esResourceSlot . at r . non [] %= (:) lastLit

encodeUniqueConstraints :: MonadState EncodedState m => Schedule -> m [PB.Constraint]
encodeUniqueConstraints = foldMapM (updateState . nodeUnique') . combinedList

encodePrecedenceConstraints :: MonadState EncodedState m => Schedule -> m [PB.Constraint]
encodePrecedenceConstraints sc = do
  let maps = runReader toMapNodes sc
  foldMapM (fmap (filter (not . null . view _1)) . updateState . precedence maps) . combinedList $ sc

encodeResourceConstraints :: MonadState EncodedState m => Schedule -> m [PB.Constraint]
encodeResourceConstraints sc = resourceConstraint (sc^.sResources. to fromResourceList) . combinedListByStep $ sc

combinedListByStep :: Schedule -> Map Int (Map Resource [Int])
combinedListByStep = foldl' buildMapNodes M.empty . combinedList
  where 
    buildMapNodes m (n1, n2) = foldl' (toMapResource (n1^.nResource) (n1^.nId)) m [(n1^.nStartStep)..(n2^.nEndStep)]
    toMapResource r i m s = m & at s . non M.empty . at r . non [] %~ (:) i

resourceConstraint :: MonadState EncodedState m => Map Resource ResourceConf -> Map Int (Map Resource [Int]) -> m [PB.Constraint]
resourceConstraint rs nodes = foldlM addConstraint [] (M.keys nodes)
  where 
    addConstraint :: MonadState EncodedState m => [PB.Constraint] -> Int -> m [PB.Constraint]
    addConstraint xs step = (<>xs) <$> foldlM (resourceToConstraint step) [] (M.keys (nodes M.! step))

    resourceToConstraint :: MonadState EncodedState m => Int -> [PB.Constraint] -> Resource -> m [PB.Constraint]
    resourceToConstraint step rxs resource = do
      let resWeight = runReader (resourceWeight $ Just 1) (rs M.! resource)
      encoded <- get
      return $ ([ (toInteger rw, [vr]) | vr <- encoded ^. esResourceSlot . to (M.! resource) . to sort
                                      , rw <- resWeight^..folded . _1
               ] 
               <> [(toInteger $ negate step, [(nodeId * 10) + step]) | nodeId <- nodes M.! step M.! resource]
               , PB.Ge, 0)
               : rxs

fromResourceList :: ResourceList -> Map Resource ResourceConf
fromResourceList (ResourceList l) = fromList . fmap (\x -> (x ^. rcResource, x)) $ l

combinedList :: Schedule -> [(Node, Node)]
combinedList sc = zip (sc ^. sAsap) (sc ^. sAlap)

toMapNodes :: Reader Schedule (Map Int Node, Map Int Node)
toMapNodes = do
  asap <- view sAsap
  alap <- view sAlap
  return (fromNodes asap, fromNodes alap)

fromNodes :: [Node] -> Map Int Node
fromNodes = fromList . fmap (\x -> (x ^. nId, x))

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

updateState :: MonadState EncodedState m => (Int, [PB.Constraint]) -> m [PB.Constraint]
updateState fn = do
  let (x', constraints) = fn
  modify (over esLastLit (max x'))
  return constraints

calculateMaxX :: [PB.WeightedTerm] -> Int
calculateMaxX list = let maxL = (maximumOf traverse list ^.. folded . _2 . folded) & firstOf traverse
                      in maybe minInt identity maxL


resources' :: Reader ResourceList [(Integer, Resource)]
resources' = magnify (_Wrapped' . folded) $ resourceWeight Nothing

resourceWeight :: Maybe Int -> Reader ResourceConf [(Integer, Resource)]
resourceWeight maybeWeight = do 
    weight <- view rcWeight
    amount <- view rcAmount
    rType <- view rcResource
    let w = maybe weight identity maybeWeight
    return $ foldl' (createResourceWeight w rType) [] [0 .. amount -1]

createResourceWeight :: Int -> Resource -> [(Integer, Resource)] -> Int -> [(Integer, Resource)]
createResourceWeight weight rType pbsum b =
  (fromIntegral ((2 ^ b) * weight), rType) : pbsum
