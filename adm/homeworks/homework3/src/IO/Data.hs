{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}

module IO.Data where

import           Control.Arrow
import           Data.List
import           Data.Text     (Text)
import qualified Data.Text     as L
import qualified Data.Text.IO  as LIO
import           Data.Types

data DataSet a =
  DataSet
    { trainingData :: [a]
    , testData     :: [a]
    }
  deriving (Show)

instance Functor DataSet where
  fmap f DataSet {..} = DataSet (fmap f trainingData) (fmap f testData)

instance Semigroup (DataSet a) where
  (<>) d1 d2 =
    DataSet
      { trainingData = trainingData d1 <> trainingData d2
      , testData = testData d1 <> testData d2
      }

instance Monoid (DataSet a) where
  mempty = DataSet [] []

data KDTreeInput =
  KDTreeInput
    { input :: Input
    , tuples :: Tuple15 Int
    }
  deriving (Show)

data Input =
  Input
    { idx    :: Int
    , labels :: [Text]
    }
  deriving (Show)

toLSH :: Input -> (Int, [Text])
toLSH = idx &&& labels

searchValue :: Int -> DataSet a -> a
searchValue idx DataSet {..} = trainingData !! (idx - 1)

zipDataSet :: [a] -> DataSet b -> DataSet (a, b)
zipDataSet as DataSet {..} = DataSet (zip as trainingData) (zip as testData)

ioForLSH :: IO (DataSet Input)
ioForLSH = ioToWords

ioForKDTree :: IO (DataSet KDTreeInput)
ioForKDTree = toTuple15DataSet . normalize <$> ioToWords

toTuple15DataSet ::
     DataSet (Int, [(Int, Text)]) -> DataSet KDTreeInput
toTuple15DataSet =
  fmap (\(idx, points) -> KDTreeInput (Input idx (snd <$> filter ((/=) 0 . fst) points)) (toTuple15 (fst <$> points)))

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
  zipDataSet [1 ..] .
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
