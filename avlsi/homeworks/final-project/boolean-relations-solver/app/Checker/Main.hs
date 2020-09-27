{-|
Module      : Main
Description : Program Main
Copyright   : (c) Juan Pablo Royo Sales, 2020
License     : GPL-3
Maintainer  : juanpablo.royo@gmail.com
Stability   : educational
Portability : POSIX

This program is a sanity check to test if the outputs generated are optimum
according to results.txt file

-}
module Main where

import           Relude
import           System.Directory
import           Text.Trifecta    as T

isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

skipHSpaces :: CharParsing m => m ()
skipHSpaces = skipSome (satisfy isHSpace)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

findFileName :: Parser a -> Parser a
findFileName p = string "==>" *> skipHSpaces *> p <* string "inp" <* skipHSpaces <* string "<=="

parseFileName :: Parser [Char]
parseFileName = findFileName (manyTill anyChar (char '.'))

parseOptimum :: Parser Integer
parseOptimum = integer

parsePair :: Parser (Text, Integer)
parsePair = do
  file <- parseFileName
  skipEOL
  optimum <- parseOptimum
  skipEOL
  return (toText $ "out/" <> file <> ".out", optimum)

parseResults :: Parser [(Text, Integer)]
parseResults = many parsePair <* eof

parseHeader :: Parser Integer
parseHeader = some digit *> space *> integer

skipLines :: Integer -> Parser ()
skipLines 0 = return ()
skipLines n = do
  n' <- some digit <* some space <* some digit <* some space <* some digit <* newline
  let x = either (const 0) identity $ readEither @Integer n'
  skipLines (n-x)

parseOutput :: Parser Integer
parseOutput = do
  boxes <- parseHeader
  skipLines boxes
  integer

main :: IO ()
main = do
  content <- maybe [] identity <$> parseFromFile parseResults "results.txt"
  forM_ content $ \(f, optimum)  -> do
    whenM (doesFileExist $ toString f) $ do
      result <- maybe 0 identity <$> parseFromFile parseOutput (toString f)
      if result == optimum
        then print (f <> ": OK")
        else print (f <> ": ERROR --> OPTIMUM " <> show optimum <> " ---> MY SOLUTION " <> show result)
