module Data.Box
  ( fromInput
  , Boxes
  , Box
  , Solution(..)
  , ProposedBox(..)
  , rollWidth
  , boxes
  , rollMaxLength
  ) where

import           Control.Exception.Safe
import           GHC.Base               (String)
import           GHC.Show               as S
import           Protolude              as P

data Box =
  Box
    { amount :: !Int
    , cordX  :: !Int
    , cordY  :: !Int
    }

instance Show Box where
  show Box {..} = P.show amount <> "   " <> P.show cordX <> " " <> P.show cordY

data Boxes =
  Boxes
    { rollWidth     :: !Int
    , amountBoxes   :: !Int
    , rollMaxLength :: !Int
    , boxes         :: ![Box]
    }

instance Show Boxes where
  show Boxes {..} =
    toS $
    unlines $
    (P.show rollWidth <> " " <> (P.show $ length boxes)) : (P.show <$> boxes)

data ProposedBox =
  ProposedBox
    { xtl :: Int
    , ytl :: Int
    , xbr :: Int
    , ybr :: Int
    }

instance Show ProposedBox where
  show ProposedBox {..} =
    P.show xtl <> " " <> P.show ytl <> "   " <> P.show xbr <> " " <> P.show ybr

data Solution =
  Solution
    { lengthRoll :: Int
    , propBoxes  :: [ProposedBox]
    }

instance Show Solution where
  show Solution {..} =
    toS $ unlines $ (P.show lengthRoll) : (P.show <$> propBoxes)

maxLength :: [Box] -> Int
maxLength = getSum . foldMap (Sum . maxLengthB)

maxLengthB :: Box -> Int
maxLengthB Box {..} = amount * (max cordX cordY)

amountB :: [Box] -> Int
amountB = getSum . foldMap (Sum . amount)

checkInput :: MonadThrow m => Int -> [Box] -> m ()
checkInput amountBoxes boxes =
  when (amountBoxes /= amountB boxes) $
  throwString "Wrong file format. Amount boxes is not equal to parsed boxes"

fromInput :: IO Boxes
fromInput = do
  [width, amountBoxes] <- safeCheck . lineToHeader <$> getLine
  boxes <- safeCheck . traverse lineBox . lines <$> getContents
  checkInput amountBoxes boxes
  return $ Boxes width (length boxes) (maxLength boxes) boxes

safeCheck :: MonadThrow m => Either String (m a) -> m a
safeCheck = either throwString identity

lineToHeader :: Text -> Either String [Int]
lineToHeader = traverse (first toS . readEither . toS) . words

lineBox :: Text -> Either String Box
lineBox = join . fmap toBox . traverse (first toS . readEither . toS) . words

toBox :: [Int] -> Either String Box
toBox [amount, width, height] = Right $ Box amount width height
toBox _                       = Left "Error Parsing Box"
