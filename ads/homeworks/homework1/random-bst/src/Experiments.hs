module Experiments where


import           Test.QuickCheck
import           Data.List
import           System.Random
import           RandomBST as RBST

experiment :: StdGen -> Int -> IO String
experiment gen n = do
  number <- (generate $ choose (0, 10000000)) :: IO Int
  let list = [number..number+n-1]
  let lengthList = length list
  let action     = fromList list gen :: RTreap StdGen Int Int
  let actionDel  = RBST.delete (list !! ((lengthList `div` 2)-1)) action
  let heightA    = height action
  let heightD    = height actionDel
  let expectedHeight = logBase (2 :: Float) (fromIntegral lengthList)
  let varHeight = abs (fromIntegral heightA - expectedHeight)
  let leafD    = leafDepth action
  let avgLeafD = average $ map fromIntegral leafD :: Double
  let varLeafD = variance $ map fromIntegral leafD :: Double
  let varHeightDel = abs (fromIntegral heightD - expectedHeight)
  let leafDDel    = leafDepth actionDel
  let avgLeafDDel = average $ map fromIntegral leafDDel :: Double
  let varLeafDDel = variance $ map fromIntegral leafDDel :: Double
  let insertExpOnly = intercalate "," [show lengthList, show heightA, show expectedHeight, show varHeight, show avgLeafD, show varLeafD] <> "\n"
  let insertDelExp = intercalate "," [show lengthList, show heightD, show expectedHeight, show varHeightDel, show avgLeafDDel, show varLeafDDel] <> "\n"
  return $ insertExpOnly <> insertDelExp

generateList :: Int -> Gen [Int]
generateList n =
  (vectorOf n (choose (0, 10000000)) :: Gen [Int]) `suchThat` isUnique
  where isUnique x = nub x == x

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

variance :: Fractional a => [a] -> a
variance xs = sum (map ((^ (2 :: Int)) . subtract (average xs)) xs)
  / fromIntegral (length xs - 1)

