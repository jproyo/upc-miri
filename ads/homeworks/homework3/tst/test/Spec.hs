import           Data.Tree.TST
import           Protolude
import           Test.QuickCheck
import qualified Data.Set as S (size, fromList)

main :: IO ()
main = do
  putText "\nInsertion with random lists from 20 to 2000 elems cases"
  quickCheck $ withMaxSuccess 200 $ forAll genList insertionRandom

insertionRandom :: [[Char]] -> Property
insertionRandom xs =
  length xs >= 20 ==> cover 30 (length xs < 100) "Less than 100 elems" $
  cover 30 (length xs > 100 && length xs < 1000) "Between 100 and 1000" $
  cover 40 (length xs > 1000) "Between 1000 and 2000" $
    (countWords $ fromList xs) === (S.size $ S.fromList xs)


genList :: Arbitrary a => Gen [a]
genList = choose (50, 5000) >>= flip vectorOf arbitrary


