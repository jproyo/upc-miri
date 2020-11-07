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
  { _srNodes     :: [NodeResult]
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

instance Show ScheduleResult where
  show ScheduleResult{..} = 
    let nodes = L.groupBy (\a b -> a^.nrStep == b^.nrStep) . sortWith (view nrStep) $ _srNodes
     in toString $ "\n---------------------------\n" <> showNodes nodes <> "\n---------------------------\n" <> showResource _srResources <> "\n---------------------------\n" <> "Optimum: " <> R.show _srOptimum

showResource :: [(ResourceType, Int)] -> Text
showResource = mappend "###### Resources ######\n" . T.intercalate "\n" . R.foldl' eachResource [] 
  where
    eachResource :: [Text] -> (ResourceType, Int) -> [Text]
    eachResource accum (r, amount) = R.show r <> ": " <> R.show amount : accum

showNodes :: [[NodeResult]] -> Text
showNodes = mappend "###### Schedule ######" . R.foldl' eachStep "" 
  where
    eachStep :: Text -> [NodeResult] -> Text
    eachStep accum xs = accum <> "\n" <> "Step " <> (R.show . _nrStep . L.head $ xs) <> ": " <> toText (T.intercalate " | " $ R.reverse $ R.foldl' eachNode [] xs)

    eachNode :: [Text] -> NodeResult -> [Text]
    eachNode accum NodeResult{..} = "[" <> R.show _nrId <> maybe "]" (mappend "]" . mappend " --> " . R.show) _nrConnectedToNode : accum

instance Wrapped ResourceList

maxNode :: Schedule -> Int
maxNode sc = let alapMax = maximumOf traverse (sc^.sAlap ^.. folded . nId) & maybe 0 identity
                 alapMaxStep = maximumOf traverse (sc^.sAsap ^.. folded . nEndStep) & maybe 0 identity
              in (alapMax * 10)+alapMaxStep
