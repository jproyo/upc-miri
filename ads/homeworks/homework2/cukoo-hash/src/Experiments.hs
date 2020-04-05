{-# LANGUAGE TypeApplications #-}

module Experiments where

import           Control.Monad.ST
import           Data.Hash.Cukoo  as C
import           Test.QuickCheck

experiment :: Int -> IO String
experiment n = do
  idx    <- getPositive <$> generate (arbitrary @(Positive Int))
  list <- generate $ shuffle [idx..idx+n]
  print list
  (r1, r2) <- runST $ do
    hash <- create
    _ <- mapM (insert hash) $ list
    size <- C.elements hash
    l    <- toList hash
    pure $ return (size, l)
  return $ ("Size: "<>show r1<>" - List: " <> show r2)


--
--data Meassure = Meassure
--  { sizeList :: Int
--  , empHeight  :: Int
--  , expHeight :: Double
--  , avgLeaf :: Double
--  }
--
--instance Show Meassure where
--  show Meassure {..} = intercalate
--    ","
--    [show sizeList, show empHeight, show expHeight, show avgLeaf] <> "\n"
--
--experiment' :: RandomGen g => RTreap g Int Int -> Meassure
--experiment' rbst =
--  let listL = nodes rbst
--  in  Meassure { sizeList  = listL
--               , empHeight = height rbst
--               , expHeight = expectedHeight listL
--               , avgLeaf   = average $ map fromIntegral $ leafDepth rbst
--               }
--
--expectedHeight :: Int -> Double
--expectedHeight = logBase 2 . fromIntegral
--
--average :: Fractional a => [a] -> a
--average xs = sum xs / fromIntegral (length xs)
--
