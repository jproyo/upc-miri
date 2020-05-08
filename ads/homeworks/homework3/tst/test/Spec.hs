import           Data.Tree.TST
import           Test.QuickCheck

main :: IO ()
main = do
  gen <- newStdGen
  putStrLn "\nInsertion with random lists from 20 to 2000 elems cases"
  quickCheck $ withMaxSuccess 200 $ insertionRandom gen
  putStrLn "\nDeletion with random lists from 20 to 2000 elems cases"
  quickCheck $ withMaxSuccess 200 $ deletionRandom gen
  putStrLn
    "\nInsertion with random insertion after first build on random lists from 20 to 2000 elems cases"
  quickCheck $ withMaxSuccess 200 $ buildAndInsertRandom gen

insertionRandom :: StdGen -> ListArrayTest -> Property
insertionRandom gen xs =
  lengthA xs >= 20 ==> cover 30 (lengthA xs < 100) "Less than 100 elems" $
  cover 30 (lengthA xs > 100 && lengthA xs < 1000) "Between 100 and 1000" $
  cover 40 (lengthA xs > 1000) "Between 1000 and 2000" $
  let list = unList xs
      lengthList = lengthA xs
      action = height (fromList list gen :: RTreap StdGen Int Int)
      result = fromIntegral action
   in result <= expected lengthList
  where
    types = gen :: StdGen

deletionRandom :: StdGen -> ListArrayTest -> Property
deletionRandom gen xs =
  lengthA xs >= 20 ==> cover 30 (lengthA xs < 100) "Less than 100 elems" $
  cover 30 (lengthA xs > 100 && lengthA xs < 1000) "Between 100 and 1000" $
  cover 40 (lengthA xs > 1000) "Between 1000 and 2000" $
  let list = unList xs
      lengthList = (lengthA xs - 1)
      toDelete = list !! ((lengthA xs `div` 2) - 1)
      action =
        height . RandomBST.delete toDelete $
        (fromList list gen :: RTreap StdGen Int Int)
      result = fromIntegral action
   in result <= expected lengthList
  where
    types = gen :: StdGen

buildAndInsertRandom ::
     StdGen -> ListArrayTest -> Positive (Small Int) -> Property
buildAndInsertRandom gen xs elem =
  lengthA xs >= 20 ==> cover 30 (lengthA xs < 100) "Less than 100 elems" $
  cover 30 (lengthA xs > 100 && lengthA xs < 1000) "Between 100 and 1000" $
  cover 40 (lengthA xs > 1000) "Between 1000 and 2000" $
  let newX = getSmall . getPositive $ elem
      list = unList xs
      lengthList = lengthA xs
      action =
        height . RandomBST.insert newX $
        (fromList list gen :: RTreap StdGen Int Int)
      result = fromIntegral action
   in result <= expected lengthList
  where
    types = gen :: StdGen

expected lengthList =
  logBase (2 :: Float) (fromIntegral lengthList :: Float) * 3
