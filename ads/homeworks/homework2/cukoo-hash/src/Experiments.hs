{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Experiments where

--------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.ST
import           Data.Hash.Cukoo       as C
import           Data.List
import           Data.Maybe
import           Data.Time
import           Data.Time.Clock.POSIX
import           Prelude               as P
import           System.IO.Unsafe
import           Test.QuickCheck       as T

--------------------------------------------------------------------------------
data Meassure =
  Meassure
    { sizeList :: !Int
    , hashtableSize :: !Int
    , avg      :: !Integer
    }

instance Show Meassure where
  show Meassure {..} = intercalate "," [show sizeList, show hashtableSize, show avg] <> "\n"


runAvgRehashes :: IO String
runAvgRehashes = do
  idx <- getPositive <$> generate ((arbitrary @(Positive Int)) `suchThat` (flip (<) 10000 . getPositive))
  list <- generate $ shuffle [idx .. idx + 1000]
  result <- mapM avgRehashes list
  return $ foldMap show result

avgRehashes :: Int -> IO Meassure
avgRehashes n = do
  (l, r) <- avgRehashes' n
  return $ Meassure {hashtableSize = l, sizeList = n, avg = fromIntegral r}

avgRehashes' :: Int -> IO (Int, Int)
avgRehashes' n = do
  idx <- getPositive <$> generate (arbitrary @(Positive Int))
  list <- generate $ shuffle [idx .. idx + n]
  return $
    runST $ do
      hash <- create
      mapM_ (C.insert hash) $ list
      rehashes <- C.rehashesCount hash
      l        <- C.length hash
      return (l, rehashes)

runInsertWithoutRehash :: IO String
runInsertWithoutRehash = do
  result <-
    replicateM 3000 $ do
      n <-
        generate
          (T.elements [100, 200, 600, 800, 1000, 2000, 6000, 8000, 10000])
      insertWithoutRehash n
  return $ foldMap show result

insertWithoutRehash :: Int -> IO Meassure
insertWithoutRehash n = do
  result <- insertWithoutRehash' n
  return
    Meassure
      {hashtableSize = 0, sizeList = n, avg = (sum result) `div` (fromIntegral (P.length result))}

insertWithoutRehash' :: Int -> IO [Integer]
insertWithoutRehash' n = do
  idx <- getPositive <$> generate (arbitrary @(Positive Int))
  list <- generate $ shuffle [idx .. idx + n]
  let (test, train) = splitAt (n `div` 2) list
  fmap catMaybes <$> return $
    runST $ do
      hash <- create
      mapM_ (C.insert hash) $ test
      forM train $ \e -> do
        start <- toNano <$> return (unsafePerformIO getCurrentTime)
        _ <- C.insert hash e
        rehash <- C.rehashed hash
        end <- toNano <$> return (unsafePerformIO getCurrentTime)
        return $ unsafePerformIO $ print start
        let diff = (end - start)
        return $
          if rehash
            then Nothing
            else Just diff

toNano :: UTCTime -> Integer
toNano = floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
