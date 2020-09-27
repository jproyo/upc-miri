{-|
Module      : BoxWrapping
Description : This module has the purpose to hide the rest of the modules and expose only the minimum thing needed
Copyright   : (c) Juan Pablo Royo Sales, 2020
License     : GPL-3
Maintainer  : juanpablo.royo@gmail.com
Stability   : educational
Portability : POSIX

-}


module BoxWrapping
  ( module Data.Box
  , module SAT.Solver
  , module S
  ) where

import           Data.Box
import           SAT.Solver
import           SAT.Types  as S (ProgOptions (..), AmoEncoder(..))
