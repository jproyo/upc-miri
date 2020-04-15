{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module IO.Data where

import Control.Arrow
import Control.DeepSeq
import Data.List
import Data.Text (Text)
import qualified Data.Text as L
import qualified Data.Text.IO as LIO
import Data.Types
import GHC.Generics (Generic)

data DataSet a =
  DataSet
    { trainingData :: [a]
    , testData :: [a]
    }
  deriving (Show, Generic, NFData)

instance Functor DataSet where
  fmap f DataSet {..} = DataSet (fmap f trainingData) (fmap f testData)

class Search a where
  match :: a -> Int -> Bool

data KDTreeInput =
  KDTreeInput
    { input :: Input
    , tuples :: Tuple15 Int
    }
  deriving Show

instance NFData KDTreeInput where
  rnf = rnf . input

instance Search KDTreeInput where
  match k n = match (input k) n

data Input =
  Input
    { idx :: Int
    , labels :: [Text]
    }
  deriving (Show, Generic, NFData)

instance Search Input where
  match Input {..} n = idx == n

toLSH :: Input -> (Int, [Text])
toLSH = idx &&& labels

searchValue :: Search a => Int -> DataSet a -> Maybe a
searchValue idx DataSet {..} = find (flip match idx) trainingData

zipDataSet :: DataSet b -> DataSet (Int, b)
zipDataSet DataSet {..} =
  DataSet (zip [1 ..] trainingData) (zip [1 ..] testData)

ioForLSH :: IO (DataSet Input)
ioForLSH = ioToWords

ioForKDTree :: IO (DataSet KDTreeInput)
ioForKDTree = toTuple15DataSet . normalize <$> ioToWords

toTuple15DataSet :: DataSet (Int, [(Int, Text)]) -> DataSet KDTreeInput
toTuple15DataSet =
  fmap
    (\(idx, points) ->
       KDTreeInput
         (Input idx (snd <$> filter ((/=) 0 . fst) points))
         (toTuple15 (fst <$> points)))

normalize :: DataSet Input -> DataSet (Int, [(Int, Text)])
normalize = fmap (idx &&& (bitVal . labels))

toBitVal :: [Text] -> Text -> [(Int, Text)] -> [(Int, Text)]
toBitVal l e acc = (maybe (0, e) (const (1, e)) $ elemIndex e l) : acc

bitVal :: [Text] -> [(Int, Text)]
bitVal l = foldr (toBitVal l) [] categories

toBits :: [Text] -> [Int]
toBits = fmap fst . bitVal

ioToWords :: IO (DataSet Input)
ioToWords =
  fmap (uncurry Input) .
  zipDataSet .
  uncurry DataSet . (take 800 &&& drop 800) . fmap L.words . L.lines <$>
  LIO.readFile "./input/data.in"

categories :: [Text]
categories =
  [ "freshmeat"
  , "dairy"
  , "confectionery"
  , "Male"
  , "DoNotOwnHome"
  , "Female"
  , "cannedveg"
  , "frozenmeal"
  , "beer"
  , "fish"
  , "wine"
  , "Homeowner"
  , "fruitveg"
  , "softdrink"
  , "cannedmeat"
  ]
