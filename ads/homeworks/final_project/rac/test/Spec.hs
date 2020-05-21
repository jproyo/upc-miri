import           Control.Monad.Random hiding (fromList)
import           Data.Zipper.Random
import           Protolude
import           System.IO.Unsafe
import           Test.QuickCheck

main :: IO ()
main = do
  putText "\nInsertion with random lists from 20 to 2000 elems cases"
  quickCheck $ withMaxSuccess 200 $ forAll genList insertionRandom

insertionRandom :: [Text] -> Property
insertionRandom xs =
  length xs >= 20 ==> cover 30 (length xs < 100) "Less than 100 elems" $
  cover 30 (length xs > 100 && length xs < 1000) "Between 100 and 1000" $
  cover 40 (length xs > 1000) "Between 1000 and 2000" $
  (length $ unsafePerformIO $ fromList xs) === (length xs)

genList :: Gen [Text]
genList = choose (50, 5000) >>= flip vectorOf (toS <$> arbitrary @[Char])
