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

import Relude

type Asap = [Node]

type Alap = [Node]

data Schedule = Schedule
  { asap :: Asap,
    alap :: Alap,
    resources :: ResourceList
  }
  deriving (Eq, Show)

data Node = Node
  { resource :: Resource,
    id :: Integer,
    startStep :: Integer,
    toNode :: Maybe Integer
  }
  deriving (Show, Eq)

data Resource
  = Adder
  | Multplier
  | Substracter
  | Comparator
  deriving (Show, Eq)

newtype ResourceList = ResourceList [(Resource, Integer)]
  deriving (Show, Eq)
  deriving newtype (Semigroup, Monoid)
