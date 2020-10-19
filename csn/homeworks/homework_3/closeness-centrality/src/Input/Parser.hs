{-# LANGUAGE ParallelListComp #-}
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
import Relude

parsedInts :: LByteString -> Maybe (Integer, Integer)
parsedInts = toTuple . traverse (readMaybe @Integer . decodeUtf8) . B.words

toTuple :: Maybe [Integer] -> Maybe (Integer, Integer)
toTuple (Just [a,b]) = Just (a,b)
toTuple _     = Nothing

parseTerms :: [LByteString] -> Maybe [(Text, Text)]
parseTerms contents = Just [ (x,y) | [x,y] <- (fmap decodeUtf8 . B.words) <$> contents, x /= y]

parse :: IO [Language]
parse =
    fmap (maybe [] identity . sequence) . mapM parseLang =<< listDirectory "data"

parseLang :: FilePath -> IO (Maybe Language)
parseLang langPath = do
    (header:contents) <- B.lines <$> readFileLBS ("data" </> langPath)
    return $ ((uncurry (Language (extractLang langPath))) <$> (parsedInts header) <*> parseTerms contents)
    where
        extractLang = toText . Relude.takeWhile (/= '_')

