-- |
-- Module      : Data.Schedule
-- Description : Main Schedule Data types domain
-- Copyright   : (c) Juan Pablo Royo Sales, 2020
-- License     : GPL-3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : educational
-- Portability : POSIX
--
-- In this module it is defined the different Data Types that support the model of Schedule Optimization Problem
module Data.Schedule where

import Control.Lens
import Data.Map as M
import Control.Arrow
import Data.List as L
import Data.Text as T
import Relude as R
import GHC.Show 

type Asap = [Node]

type Alap = [Node]

data Schedule = Schedule
  { _sAsap :: Asap,
    _sAlap :: Alap,
    _sResources :: ResourceList
  }
  deriving (Eq, Show)

data Node = Node
  { _nResource :: ResourceType,
    _nId :: Int,
    _nStartStep :: Int,
    _nEndStep :: Int,
    _nToNode :: Maybe Int
  }
  deriving (Show, Eq)

data ResourceType
  = Adder
  | Multiplier
  | Substracter
  | Comparator
  deriving (Show, Eq, Ord)

data Resource = Resource
  { _rcResource :: ResourceType,
    _rcWeight :: Int,
    _rcAmount :: Int
  }
  deriving (Show, Eq)

newtype ResourceList = ResourceList [Resource]
  deriving (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)

data NodeResult = NodeResult
  { _nrId :: Int
  , _nrStep :: Int
  , _nrResource :: ResourceType
  , _nrConnectedToNode :: Maybe Int
  } deriving Show

data ScheduleResult = ScheduleResult 
  { _srOrig      :: Schedule
  , _srNodes     :: [NodeResult]
  , _srResources :: [(ResourceType, Int)]
  , _srOptimum   :: Integer
  }

data EncodedState = EncodedState
  { _esLastLit :: Int
  , _esResourceSlot :: Map ResourceType [Int]
  } deriving (Show, Eq)

makeLenses ''NodeResult
makeLenses ''EncodedState
makeLenses ''ScheduleResult
makeLenses ''Resource
makeLenses ''ResourceList
makeLenses ''Node
makeLenses ''Schedule
makePrisms ''ResourceType

instance Wrapped ResourceList

fromResourceList :: ResourceList -> Map ResourceType Resource
fromResourceList = M.fromList . fmap (view rcResource &&& identity) . view _Wrapped'

fromNodeList :: Getter s Int -> Getter s Int -> [s] -> Map Int [Int]
fromNodeList getId getStep = M.fromListWith (++) . fmap (view getStep &&& pure . view getId)

scheduleToDot :: Text -> [Node] -> Text
scheduleToDot name sc = "digraph G { \n label=\"" <> name <> "\"\n" <> toInnerSchedule sc <> "\n\n}"

resultToDot :: Text -> ScheduleResult -> Text
resultToDot name sc = "digraph G { \n label=\"" <> name <> "\"\n" <> toInnerResultDot sc <> "\n\n}"

fromNodeScheduleList :: [Node] -> Map Int [Node]
fromNodeScheduleList = M.fromListWith (++) . fmap (view nStartStep &&& pure)

toInnerSchedule :: [Node] -> Text
toInnerSchedule s = let clusters = stepsSubgraph . fromNodeList nId nStartStep $ s
                        edges    = edgesConn . fmap (view nId &&& view nToNode) $ s
                     in clusters <> "\n" <> edges

toInnerResultDot :: ScheduleResult -> Text
toInnerResultDot s = let clusters = s^.srNodes . to (stepsSubgraph . fromNodeList nrId nrStep)
                         edges    = s^.srNodes . to (edgesConn . fmap (view nrId &&& view nrConnectedToNode))
                      in clusters <> "\n" <> edges

edgesConn :: [(Int, Maybe Int)] -> Text
edgesConn = T.intercalate "\n" . R.filter (not . T.null) . fmap toEdge

toEdge :: (Int, Maybe Int) -> Text
toEdge (nId', toId) = maybe "" toConnectedEdge toId
  where
    toConnectedEdge :: Int -> Text
    toConnectedEdge toId' = " " <> R.show nId' <> " -> " <> R.show toId'

stepsSubgraph :: Map Int [Int] -> Text
stepsSubgraph = M.foldrWithKey subgraph ""
  where
    subgraph :: Int -> [Int] -> Text -> Text
    subgraph step nodes acc = let line = " subgraph cluster_step_" 
                                         <> R.show step 
                                         <> " { label=\"step "
                                         <> R.show step 
                                         <> "\" rank=same style=dotted color=black " 
                                         <> R.foldl' (\b a -> R.show a <> " " <> b) "" nodes
                                         <> " }"
                               in acc <> "\n" <> line 

writeDotFile :: Text -> Text -> IO ()
writeDotFile name = R.writeFileText (toString name)

instance Show ScheduleResult where
  show ScheduleResult{..} = 
    let nodes = L.groupBy (\a b -> a^.nrStep == b^.nrStep) . sortWith (view nrStep) $ _srNodes
     in toString 
        $ "\n---------------------------\n" 
        <> showNodes nodes 
        <> "\n--------------------------\n" 
        <> showResource _srOrig _srResources
        <> "\n--------------------------\n" 
        <> "Optimum: " <> R.show _srOptimum

showResource :: Schedule -> [(ResourceType, Int)] -> Text
showResource sc = mappend "###### Resources ######\n" . T.intercalate "\n" . R.foldl' (eachResource (fromResourceList (sc^.sResources))) []
  where
    eachResource :: Map ResourceType Resource -> [Text] -> (ResourceType, Int) -> [Text]
    eachResource mr accum (r, amount) = R.show r <> ": " <> R.show (amount*(mr M.! r)^.rcWeight) : accum

showNodes :: [[NodeResult]] -> Text
showNodes = mappend "###### Schedule ######" . R.foldl' eachStep "" 
  where
    eachStep :: Text -> [NodeResult] -> Text
    eachStep accum xs = accum <> "\n" <> "Step " <> (R.show . _nrStep . L.head $ xs) <> ": " <> toText (T.intercalate " | " $ R.reverse $ R.foldl' eachNode [] xs)

    eachNode :: [Text] -> NodeResult -> [Text]
    eachNode accum NodeResult{..} = "[" <> R.show _nrId <> maybe "]" (mappend "]" . mappend " --> " . R.show) _nrConnectedToNode : accum

maxNode :: Schedule -> Int
maxNode sc = let alapMax = maximumOf traverse (sc^.sAlap ^.. folded . nId) & maybe 0 identity
                 alapMaxStep = maximumOf traverse (sc^.sAsap ^.. folded . nEndStep) & maybe 0 identity
              in (alapMax * 10)+alapMaxStep
