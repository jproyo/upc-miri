module Experiments
  ( experimentMillion
  , experimentSeq
  ) where

----------------------------------------------------------------------------------

import qualified Data.FingerTree       as F
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Zipper.Random    as R
import qualified GHC.Show              as S
import           Protolude
import           System.IO             as SI
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
      (show <$> [sizeList, timeInMsRaz, timeInMsFinger])

oneMillion :: Int
oneMillion = 1000000

experimentGeneric :: Handle -> Int -> R.Raz Int -> F.FingerTree (Sum Int) Int -> IO (R.Raz Int, F.FingerTree (Sum Int) Int, Int)
experimentGeneric h num initR initF = do
  number <- (generate $ choose (1, 10000000)) :: IO Int
  ls <- shuffleM [number .. number + oneMillion - 1]
  let actualSize = oneMillion+num
  (time', f) <- withTimeInMs $ \g -> fst $ insertFinger' g ls num 0 initF
  (time, r) <- withTimeInMs $ \g -> fst $  R.insertL' g ls num 0 initR
  SI.hPutStrLn h $ show $ Meassure actualSize time time'
  SI.hFlush h
  return (r, f, actualSize)

experimentMillion :: Handle -> IO ()
experimentMillion h = do
  flip foldM_ (R.singleton 0, F.singleton 0, 0) (\(r', f', num) _ -> experimentGeneric h num r' f') ([1..100]::[Int])

experimentSeq :: Handle -> IO ()
experimentSeq h = forM_ [10000,20000..1000000] $ \n -> do
  number <- (generate $ choose (0, 10000000)) :: IO Int
  ls <- shuffleM [number .. number + n - 1]
  (time', _) <- withTimeInMs $ flip fromListToFinger ls
  (time, _) <- withTimeInMs $ fst . flip R.fromListToRaz ls
  SI.hPutStrLn h $ show $ Meassure n time time'
  SI.hFlush h

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
fromListToFinger g ls = fst $ insertFinger' g ls 0 0 F.empty

insertFinger' :: F.Measured (Sum Int) a => StdGen -> [a] -> Int -> Int -> F.FingerTree (Sum Int) a -> (F.FingerTree (Sum Int) a, StdGen)
insertFinger' g [] _ _ sq = (sq, g)
insertFinger' g (x:xs) lowBound sz sq = let (p, g') = randomR (lowBound, sz) g
                                            (left, right) = F.split ((>) p . getSum) sq
                                            sq' = (left F.|> x) F.>< right
                                         in insertFinger' g' xs lowBound (sz+1) sq'


