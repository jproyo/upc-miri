{-# LANGUAGE BlockArguments #-}
-- |
-- Module      : Input.Parser
-- Description : Parser Input
-- Copyright   : (c) Juan Pablo Royo Sales, 2020
-- License     : GPL-3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : educational
-- Portability : POSIX
--
-- This is a parser on the input file that converts to specific Data Type
module Input.Parser where

import Data.ByteString.Lazy.Char8 as B
import Data.Lang
import System.Directory
import System.FilePath.Posix
import Relude as R
import qualified Data.Map as M

parsedInts :: LByteString -> Maybe Integer
parsedInts = toTuple . traverse (readMaybe @Integer . decodeUtf8) . B.words

toTuple :: Maybe [Integer] -> Maybe Integer
toTuple (Just [a,_]) = Just a
toTuple _     = Nothing


data FoldIndexes = FoldIndexes
    { _map     :: M.Map Text Int
    , _lastIdx :: Int
    }

initialState :: FoldIndexes
initialState = FoldIndexes M.empty (-1)

parseTerms :: [LByteString] -> [(Int, Int)]
parseTerms = fst . flip runState initialState . foldlM convertToIndexes []

convertToIndexes :: MonadState FoldIndexes m => [(Int, Int)] -> LByteString -> m [(Int, Int)]
convertToIndexes accum line = do
    case parseLine line of
        Nothing -> return accum
        Just (x,y) -> if x == y
                        then return accum
                        else updateIndexes accum x y

parseLine :: LByteString -> Maybe (Text, Text)
parseLine l = case R.words . decodeUtf8 $ l of
                [x,y] -> Just (x,y)
                _     -> Nothing

updateIndexes :: MonadState FoldIndexes m => [(Int, Int)] -> Text -> Text -> m [(Int, Int)]
updateIndexes accum x y = do
    s <- get
    let c = _lastIdx s
    let m = _map s
    case M.lookup x m of
        Just xInt -> case M.lookup y m of
            Just yInt -> return $ (xInt, yInt) : accum
            Nothing   -> do
                put $ s {_lastIdx = c + 1, _map = M.insert y (c+1) m}
                return $ (xInt, c+1) : accum
        Nothing   -> case M.lookup y m of
            Just yInt -> do
                put $ s {_lastIdx = c + 1, _map = M.insert x (c+1) m}
                return $ (c+1, yInt) : accum
            Nothing   -> do
                let xInt = c + 1
                let yInt = c + 2
                put $ s {_lastIdx = yInt, _map = M.insert y yInt (M.insert x xInt m)}
                return $ (xInt, yInt) : accum


parse :: IO [Language]
parse =
    fmap (maybe [] identity . sequence) . mapM parseLang =<< listDirectory "data"

parseLang :: FilePath -> IO (Maybe Language)
parseLang langPath = do
    (header:contents) <- B.lines <$> readFileLBS ("data" </> langPath)
    return $ (Language (extractLang langPath) <$> parsedInts header <*> pure (parseTerms contents))
    where
        extractLang = toText . R.takeWhile (/= '_')

