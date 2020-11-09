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
module Encoder.Encoder where

import Control.Lens
import qualified Data.PseudoBoolean as PB
import Data.Schedule
import Data.Set.Lens
import Control.Monad
import qualified Data.Map as M
import Relude

newtype ResultEncoder = ResultEncoder (PB.Formula, EncodedState)

class (MonadReader Schedule m, MonadState EncodedState m) => Encoder m where
  encodeObjectiveFunction :: m PB.Sum
  encodeConstraints :: m [PB.Constraint]
  resourceToConstraint :: Map ResourceType Resource -> Map Int (Map ResourceType [Int]) -> Int -> [PB.Constraint] -> ResourceType -> m [PB.Constraint]

newtype TimeScheduler a = TimeScheduler { unTime :: ReaderT Schedule (State EncodedState) a }
  deriving newtype (Functor, Applicative, Monad, MonadState EncodedState, MonadReader Schedule)

instance Encoder TimeScheduler where
  encodeObjectiveFunction = ask >>= \sc -> sc^.sResources . to (runReader resourcesTime) & traverseOf each convertToSum
    where
      convertToSum (i, r) = (i,) . flip (:) [] . flip (^.) esLastLit <$> (modify (execState (updateResourceEncodedState r)) >> get)

  encodeConstraints = mconcat <$> sequence [ encodeUniqueConstraints
                                           , encodePrecedenceConstraints
                                           , encodeResourceConstraints ]

  resourceToConstraint rs nodes step rxs resource = do
      let resWeight = runReader (resourceWeight $ Just 1) (rs M.! resource)
      encoded <- get
      return $ ([ (toInteger rw, [vr]) | (rw, vr) <- zip (resWeight^..folded . _1) (encoded ^. esResourceSlot . to (M.! resource))] 
               <> [(negate 1, [(nodeId * 10) + step]) | nodeId <- nodes M.! step M.! resource]
               , PB.Ge, 0)
               : rxs


encodeTimeSchedule :: Schedule -> ResultEncoder
encodeTimeSchedule sc = let maxLit = maxNode sc
                            result = flip runState (EncodedState maxLit M.empty) . flip runReaderT sc . unTime $ encodeFormula 
                         in ResultEncoder result

newtype ResourceScheduler a = ResourceScheduler { unResource :: ReaderT Schedule (State EncodedState) a }
  deriving newtype (Functor, Applicative, Monad, MonadState EncodedState, MonadReader Schedule)

instance Encoder ResourceScheduler where
  encodeObjectiveFunction = ask >>= \sc -> sc^.sResources . to (runReader resourcesR) & traverseOf each convertToSum
    where
      convertToSum (i, r) = (i,) . flip (:) [] . flip (^.) esLastLit <$> (modify (execState (updateResourceEncodedState r)) >> get)

  encodeConstraints = mconcat <$> sequence [ encodeUniqueConstraints
                                           , encodePrecedenceConstraints
                                           , encodeResourceConstraints
                                           , encodeLatencyConstraints 
                                           ]
  resourceToConstraint _ nodes step rxs resource = do
      let constraint = [(negate 1, [(nodeId * 10) + step]) | nodeId <- nodes M.! step M.! resource]
      return $ ( constraint
               , PB.Ge, fromIntegral . negate . length $ constraint)
               : rxs


encodeResourceSchedule :: Schedule -> ResultEncoder
encodeResourceSchedule sc = let maxLit = maxNode sc
                                result = flip runState (EncodedState maxLit M.empty) . flip runReaderT sc . unResource $ encodeFormula 
                             in ResultEncoder result

encodeFormula :: Encoder m => m PB.Formula
encodeFormula = do 
  objective   <- Just <$> encodeObjectiveFunction
  constraints <- encodeConstraints
  PB.Formula <$> pure objective
             <*> pure constraints
             <*> (view esLastLit <$> get)
             <*> pure (length constraints)

updateResourceEncodedState :: ResourceType -> State EncodedState ()
updateResourceEncodedState r = do 
  esLastLit += 1 
  lastLit <- use esLastLit
  esResourceSlot . at r . non [] %= (:) lastLit

encodeUniqueConstraints :: Encoder m => m [PB.Constraint]
encodeUniqueConstraints = foldMapM (updateState . nodeUnique') =<< combinedList

encodePrecedenceConstraints :: Encoder m => m [PB.Constraint]
encodePrecedenceConstraints = do
  sc <- ask
  let maps = runReader toMapNodes sc
  foldMapM (fmap (filter (not . null . view _1)) . updateState . precedence maps) =<< combinedList

encodeResourceConstraints :: Encoder m => m [PB.Constraint]
encodeResourceConstraints = resourceConstraint =<< combinedListByStep

combinedListByStep :: Encoder m => m (Map Int (Map ResourceType [Int]))
combinedListByStep = foldl' buildMapNodes M.empty <$> combinedList
  where 
    buildMapNodes m (n1, n2) = foldl' (toMapResource (n1^.nResource) (n1^.nId)) m [(n1^.nStartStep)..(n2^.nEndStep)]
    toMapResource r i m s = m & at s . non M.empty . at r . non [] %~ (:) i

resourceConstraint :: Encoder m => Map Int (Map ResourceType [Int]) -> m [PB.Constraint]
resourceConstraint nodes = do 
  sc <- ask
  foldlM (addConstraint (sc^.sResources . to fromResourceList)) [] (M.keys nodes)
    where 
      addConstraint :: Encoder m => Map ResourceType Resource -> [PB.Constraint] -> Int -> m [PB.Constraint]
      addConstraint rs xs step = (<>xs) <$> foldlM (resourceToConstraint rs nodes step) [] (M.keys (nodes M.! step))

encodeLatencyConstraints :: Encoder m => m [PB.Constraint]
encodeLatencyConstraints = foldMapM nodeLatency =<< combinedList

nodeLatency :: Encoder m => (Node, Node) -> m [PB.Constraint]
nodeLatency (n1, n2) = do
  sc <- ask
  let rs = sc ^. sResources. to fromResourceList
  let resWeight = runReader (resourceWithoutWeight Nothing) (rs M.! (n1^.nResource))
  encoded <- get
  let nId1 = n1 ^. nId
  let nStart1 = n1 ^. nStartStep
  let nEnd2 = n2 ^. nEndStep
  let listResources = [ (toInteger rw, [vr]) | (rw, vr) <- zip (resWeight^..folded . _1) (encoded ^. esResourceSlot . to (M.! (n1^.nResource)))]
  let list = [(negate 1, [(nId1 * 10) + s]) | s <- [nStart1 .. nEnd2]] <> listResources
  return [(toList $ setOf folded list, PB.Ge, 0)]


fromResourceList :: ResourceList -> Map ResourceType Resource
fromResourceList (ResourceList l) = fromList . fmap (\x -> (x ^. rcResource, x)) $ l

combinedList :: Encoder m => m [(Node, Node)]
combinedList = ask >>= \sc -> return $ zip (sc ^. sAsap) (sc ^. sAlap) 

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

updateState :: Encoder m => (Int, [PB.Constraint]) -> m [PB.Constraint]
updateState fn = do
  let (x', constraints) = fn
  modify (over esLastLit (max x'))
  return constraints

calculateMaxX :: [PB.WeightedTerm] -> Int
calculateMaxX list = let maxL = (maximumOf traverse list ^.. folded . _2 . folded) & firstOf traverse
                      in maybe minInt identity maxL


resourcesTime :: Reader ResourceList [(Integer, ResourceType)]
resourcesTime = magnify (_Wrapped' . folded) $ reverse <$> resourceWeight Nothing

resourcesR :: Reader ResourceList [(Integer, ResourceType)]
resourcesR = magnify (_Wrapped' . folded) $ reverse <$> resourceWithoutWeight Nothing

resourceWeight :: Maybe Int -> Reader Resource [(Integer, ResourceType)]
resourceWeight maybeWeight = do 
    weight <- view rcWeight
    amount <- view rcAmount
    rType <- view rcResource
    let w = maybe weight identity maybeWeight
    return $ foldl' (createResourceWeight w rType) [] [0 .. amount -1]

resourceWithoutWeight :: Maybe Int -> Reader Resource [(Integer, ResourceType)]
resourceWithoutWeight maybeWeight = do 
    amount <- view rcAmount
    rType <- view rcResource
    let w = maybe 1 identity maybeWeight
    return $ foldl' (createResourceWeight w rType) [] [0 .. amount -1]


createResourceWeight :: Int -> ResourceType -> [(Integer, ResourceType)] -> Int -> [(Integer, ResourceType)]
createResourceWeight weight rType pbsum b =
  (fromIntegral ((2 ^ b) * weight), rType) : pbsum
