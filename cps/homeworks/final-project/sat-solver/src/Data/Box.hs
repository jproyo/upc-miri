{-|
Module      : Data.Box
Description : This module contains types describing Box, Boxes and parsing that from Input Stream
Copyright   : (c) Juan Pablo Royo Sales, 2020
License     : GPL-3
Maintainer  : juanpablo.royo@gmail.com
Stability   : educational
Portability : POSIX

This module contains the model of Data Types for the Input and Output data of the problem
-}

module Data.Box
  ( fromInput
  , Box(..)
  , Boxes(..)
  , BoxInfo(..)
  , Solution(..)
  , ProposedBox(..)
  , insideOfRoll
  ) where

--------------------------------------------------------------------------------

import           Control.Exception.Safe
import           GHC.Base               (String)
import           GHC.Show               as S
import           Protolude              as P

--------------------------------------------------------------------------------

data Box =
  Box
    { num    :: !Int
    , width  :: !Int
    , height :: !Int
    }
  deriving (Eq)

insideOfRoll :: Boxes -> Box -> Int -> Int -> Bool
insideOfRoll Boxes {..} Box {..} x y =
  x + width - 1 >= rollWidth || y + height - 1 >= rollMaxLength

data BoxInfo =
  BoxInfo
    { amount :: !Int
    , cordX  :: !Int
    , cordY  :: !Int
    }
  deriving (Eq)

instance Show BoxInfo where
  show BoxInfo {..} =
    P.show amount <> "   " <> P.show cordX <> " " <> P.show cordY

area :: BoxInfo -> Int
area BoxInfo {..} = cordX * cordY

instance Ord BoxInfo where
  compare b1 b2 = compare (area b2) (area b1)

data Boxes =
  Boxes
    { rollWidth     :: !Int
    , amountBoxes   :: !Int
    , rollMaxLength :: !Int
    , boxes         :: ![BoxInfo]
    , expandedBoxes :: ![Box]
    }

instance Show Boxes where
  show Boxes {..} =
    toS $
    unlines $
    (P.show rollWidth <> " " <> P.show amountBoxes) : (P.show <$> boxes)

data ProposedBox =
  ProposedBox
    { xtl :: Int
    , ytl :: Int
    , xbr :: Int
    , ybr :: Int
    }

instance Show ProposedBox where
  show ProposedBox {..} =
    P.show xtl <> " " <> P.show ytl <> "   " <> P.show xbr <> " " <> P.show ybr

data Solution =
  Solution
    { lengthRoll :: Int
    , propBoxes  :: [ProposedBox]
    }

instance Show Solution where
  show Solution {..} =
    toS $ unlines $ (P.show lengthRoll) : (P.show <$> propBoxes)

maxLength :: [BoxInfo] -> Int
maxLength = getSum . foldMap (Sum . maxLengthB)

maxLengthB :: BoxInfo -> Int
maxLengthB BoxInfo {..} = amount * (max cordX cordY)

amountB :: [BoxInfo] -> Int
amountB = getSum . foldMap (Sum . amount)

checkInput :: MonadThrow m => Int -> [BoxInfo] -> m ()
checkInput amountBoxes boxes =
  when (amountBoxes /= amountB boxes) $
  throwString "Wrong file format. Amount boxes is not equal to parsed boxes"

fromInput :: IO Boxes
fromInput = do
  [width, amountBoxes] <- safeCheck . lineToHeader <$> getLine
  boxes <- safeCheck . traverse lineBox . lines <$> getContents
  checkInput amountBoxes boxes
  let boxSorted = sort boxes
  let expandedBoxes =
        foldr
          (\BoxInfo {..} b -> replicate amount (cordX, cordY) ++ b)
          []
          boxSorted
  let expandedWithIdx =
        zipWith (\idx (x, y) -> Box idx x y) [0 ..] expandedBoxes
  return $
    Boxes width amountBoxes (maxLength boxes) boxSorted expandedWithIdx

safeCheck :: MonadThrow m => Either String (m a) -> m a
safeCheck = either throwString identity

lineToHeader :: Text -> Either String [Int]
lineToHeader = traverse (first toS . readEither . toS) . words

lineBox :: Text -> Either String BoxInfo
lineBox = join . fmap toBox . traverse (first toS . readEither . toS) . words

toBox :: [Int] -> Either String BoxInfo
toBox [amount, width, height] = Right $ BoxInfo amount width height
toBox _                       = Left "Error Parsing BoxInfo"
