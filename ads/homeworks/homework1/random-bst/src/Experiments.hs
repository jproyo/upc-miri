{-# LANGUAGE RecordWildCards #-}

module Experiments where


import           Test.QuickCheck
import           Data.List
import           System.Random
import           RandomBST                     as RBST

experiment :: StdGen -> Int -> IO String
experiment gen n = do
  number <- (generate $ choose (0, 10000000)) :: IO Int
  let list      = [number .. number + n - 1]
  let action    = fromList list gen :: RTreap StdGen Int Int
  let actionDel = RBST.delete (list !! ((n `div` 2) - 1)) action
  return $ foldMap (show . experiment') [action, actionDel]

data Meassure = Meassure
  { sizeList :: Int
  , empHeight  :: Int
  , expHeight :: Double
  , avgLeaf :: Double
  }

instance Show Meassure where
  show Meassure {..} = intercalate
    ","
    [show sizeList, show empHeight, show expHeight, show avgLeaf] <> "\n"

experiment' :: RandomGen g => RTreap g Int Int -> Meassure
experiment' rbst =
  let listL = nodes rbst
  in  Meassure { sizeList  = listL
               , empHeight = height rbst
               , expHeight = expectedHeight listL
               , avgLeaf   = average $ map fromIntegral $ leafDepth rbst
               }

expectedHeight :: Int -> Double
expectedHeight = logBase 2 . fromIntegral

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

