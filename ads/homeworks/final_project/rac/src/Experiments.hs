module Experiments where

----------------------------------------------------------------------------------

import qualified Data.FingerTree       as F
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Zipper.Random    as R
import qualified GHC.Show              as S
import           Protolude
import           System.Random
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


initRMillion :: IO (R.Raz Int)
initRMillion = do
  number <- (generate $ choose (0, 10000000)) :: IO Int
  ls <- shuffleM [number .. number + 1000000 - 1]
  g <- newStdGen
  return $ fst $ R.fromListToRaz g ls


initFMillion :: IO (F.FingerTree (Sum Int) Int)
initFMillion = do
  number <- (generate $ choose (0, 10000000)) :: IO Int
  ls <- shuffleM [number .. number + 1000000 - 1]
  g <- newStdGen
  return $ fromListToFinger g ls

experimentMillion :: R.Raz Int -> F.FingerTree (Sum Int) Int -> IO Text
experimentMillion initR initF = do
  number <- (generate $ choose (0, 10000000)) :: IO Int
  ls <- shuffleM [number .. number + 100000000 - 1]
  g <- newStdGen
  (result, _, _, _, _) <- flip foldM ("", initR, initF, g, 0) (\(b, r, f, g2, c) n -> do
     (time', (f', g'')) <- withTimeInMs' g2 $ \g' -> insertFinger'' g' n c f
     (time, (r', g''')) <- withTimeInMs' g'' $ \g' -> insertR g' n c r
     return $ ((show $ Meassure (c + 1 + 1000000) time time') <> b, r', f', g''', c+1)
     ) ls
  return result

withTimeInMs' :: NFData a => StdGen -> (StdGen -> (a, StdGen)) -> IO (Int, (a, StdGen))
withTimeInMs' g action = do
  let current = (fromInteger . round . (* 1000) . (* 1000)) <$> liftIO getPOSIXTime
                                      -- ^ ns       ^ ms
  before <- current
  let (r, g') = action g
  _ <- r `seq` return ()
  after <- current
  return ((after - before), (r, g'))

experimentSeq :: Int -> IO Text
experimentSeq n = do
  number <- (generate $ choose (0, 10000000)) :: IO Int
  ls <- shuffleM [number .. number + n - 1]
  (time', _) <- withTimeInMs $ flip fromListToFinger ls
  (time, _) <- withTimeInMs $ fst . flip R.fromListToRaz ls
  return $ show $ Meassure n time time'

withTimeInMs :: NFData a => (StdGen -> a) -> IO (Int, a)
withTimeInMs action = do
  g <- newStdGen
  let current = (fromInteger . round . (* 1000) . (* 1000)) <$> liftIO getPOSIXTime
                                      -- ^ ns       ^ ms
  before <- current
  let r = action g
  _ <- r `seq` return ()
  after <- current
  return ((after - before), r)


instance F.Measured (Sum Int) Int where
  measure = const $ Sum 1

instance NFData a => NFData (F.FingerTree v a) where
  rnf = rnf . toList

fromListToFinger :: StdGen -> [Int] -> F.FingerTree (Sum Int) Int
fromListToFinger g ls = fst $ insertFinger' g ls 0 F.empty

insertFinger' :: F.Measured (Sum Int) a => StdGen -> [a] -> Int -> F.FingerTree (Sum Int) a -> (F.FingerTree (Sum Int) a, StdGen)
insertFinger' g [] _ sq = (sq, g)
insertFinger' g (x:xs) sz sq = let (p, g') = randomR (0, sz) g
                                   (left, right) = F.split ((>) p . getSum) sq
                                   sq' = (left F.|> x) F.>< right
                                in insertFinger' g' xs (sz+1) sq'



insertFinger'' :: F.Measured (Sum Int) a => StdGen -> a -> Int -> F.FingerTree (Sum Int) a -> (F.FingerTree (Sum Int) a, StdGen)
insertFinger'' g x sz sq = let (p, g') = randomR (0, sz) g
                               (left, right) = F.split ((>) p . getSum) sq
                            in ((left F.|> x) F.>< right, g')

insertR :: RandomGen g => g -> a -> Int -> R.Raz a -> (R.Raz a, g)
insertR g x sz !r = let (p, g') = randomR (0, sz) g
                     in (R.insert g' R.L x . R.focus p . R.unfocus) $ r

