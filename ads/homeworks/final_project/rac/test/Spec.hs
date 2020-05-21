import           Control.Monad.Random hiding (fromList)
import qualified Data.Set             as S (fromList, size)
import           Data.Zipper.Random
import           Protolude
import           System.IO.Unsafe
import           Test.QuickCheck

instance Arbitrary Text where
  arbitrary = toS <$> arbitrary @[Char]

main :: IO ()
main = do
  putText "\nInsertion with random lists from 20 to 2000 elems cases"
  quickCheck $ withMaxSuccess 200 $ forAll genList insertionRandom

insertionRandom :: [Text] -> Property
insertionRandom xs =
  length xs >= 20 ==> cover 30 (length xs < 100) "Less than 100 elems" $
  cover 30 (length xs > 100 && length xs < 1000) "Between 100 and 1000" $
  cover 40 (length xs > 1000) "Between 1000 and 2000" $
  (size $ insertL xs) === (length xs)

insertL :: [Text] -> Tree Text
insertL (x:xs) = unsafePerformIO $ do
  _ <- newStdGen
  insertL' xs 0 (unfocus $ singleton x)

insertL' :: MonadRandom m => [a] -> Int -> Tree a -> m (Tree a)
insertL' [] _ r         = return r
insertL' (x:xs) sz r = do
  p <- getRandomR (0, sz)
  r <- insertAt' L p x r
  insertL' xs (sz+1) r

genList :: Arbitrary a => Gen [a]
genList = choose (50, 5000) >>= flip vectorOf arbitrary
