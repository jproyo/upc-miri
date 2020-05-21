module Experiments where

----------------------------------------------------------------------------------

import           Control.Monad.Random
import qualified Data.FingerTree       as F
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Zipper.Random    as R
import qualified GHC.Show              as S
import           Protolude
import           System.Random.Shuffle
import           Test.QuickCheck

----------------------------------------------------------------------------------
data Meassure =
  Meassure
    { sizeList       :: Int
    , timeInMsRaz    :: Int
    , timeInMsFinger :: Int
    }

instance S.Show Meassure where
  show Meassure {..} =
    intercalate
      ","
      (show <$> [sizeList, timeInMsRaz, timeInMsFinger]) <>
    "\n"

experimentSeq :: Int -> IO Text
experimentSeq n = do
  setStdGen =<< newStdGen
  number <- (generate $ choose (0, 10000000)) :: IO Int
  ls <- shuffleM [number .. number + n - 1]
  let initR = R.singleton @Int 0
  let initF = F.empty
  (time', _) <- withTimeInMs $ insertFinger' ls 0 initF
  (time, _) <- withTimeInMs $ R.insertL' ls 0 initR
--  putText $ R.showRaz r
  return $ show $ Meassure n time time'

withTimeInMs :: IO a -> IO (Int, a)
withTimeInMs action = do
  let current = (fromInteger . round . (* 1000)) <$> liftIO getPOSIXTime
  before <- current
  a <- action
  after <- current
  return ((after - before), a)


instance F.Measured (Sum Int) Int where
  measure = const $ Sum 1

insertFinger' :: (F.Measured (Sum Int) a, MonadRandom m) => [a] -> Int -> F.FingerTree (Sum Int) a -> m (F.FingerTree (Sum Int) a)
insertFinger' [] _ sq = return sq
insertFinger' (x:xs) sz sq = do
  p <- getRandomR (0, sz)
  let
    (left, right) = F.split ((>) p . getSum) sq
    sq' = (left F.|> x) F.>< right
  insertFinger' xs (sz+1) sq'
