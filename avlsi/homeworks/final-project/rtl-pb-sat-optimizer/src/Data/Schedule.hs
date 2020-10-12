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
import Relude

type Asap = [Node]

type Alap = [Node]

data Schedule = Schedule
  { _sAsap :: Asap,
    _sAlap :: Alap,
    _sResources :: ResourceList
  }
  deriving (Eq, Show)

data Node = Node
  { _nResource :: Resource,
    _nId :: Int,
    _nStartStep :: Int,
    _nEndStep :: Int,
    _nToNode :: Maybe Int
  }
  deriving (Show, Eq)

data Resource
  = Adder
  | Multiplier
  | Substracter
  | Comparator
  deriving (Show, Eq)

data ResourceConf = ResourceConf
  { _rcResource :: Resource,
    _rcWeight :: Int,
    _rcAmount :: Int
  }
  deriving (Show, Eq)

newtype ResourceList = ResourceList [ResourceConf]
  deriving (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)

makeLenses ''ResourceConf
makeLenses ''ResourceList
makeLenses ''Node
makeLenses ''Schedule
makePrisms ''Resource

instance Wrapped ResourceList
