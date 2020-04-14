module IO.Data where

import           Control.Arrow
import           Data.List
import           Data.Types

ioForLSH :: IO [(Int, [String])]
ioForLSH = zip [1 ..] <$> ioToWords

ioForKDTree :: IO [(Int, Tuple15 Int)]
ioForKDTree = fmap (second (toTuple15 . fmap fst . normalize)) <$> ioForKDTreeRef

ioForKDTreeRef :: IO [(Int, [String])]
ioForKDTreeRef = zip [1..] <$> ioToWords

normalize :: [String] -> [(Int, String)]
normalize line = foldr (toBitVal line) [] header
  where
    toBitVal l e acc = (maybe (0, e) (const (1, e)) (elemIndex e l)) : acc

ioToWords :: IO [[String]]
ioToWords = fmap words . lines <$> readFile "./input/train_data.in"

header :: [String]
header =
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

