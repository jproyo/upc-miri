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

import Data.Schedule
import Relude
--import System.Directory
import Text.Trifecta as T

parseInt :: Parser Int
parseInt = fromInteger <$> integer

parseAsap :: Parser Asap
parseAsap = manyTill parseNode (try (string "ALAP"))

parseAlap :: Parser Alap
parseAlap = manyTill parseNode (try (string "RESOURCES"))

skipAsapHeader :: Parser ()
skipAsapHeader = skipComments *> string "ASAP" *> whiteSpace <* optional newline

skipAlapHeader :: Parser ()
skipAlapHeader = whiteSpace <* optional newline

skipComments :: Parser ()
skipComments = skipMany (char '#' <* manyTill anyChar (try newline))

skipResourceListHeader :: Parser ()
skipResourceListHeader = skipComments *> whiteSpace <* optional newline

parseResourceAmount :: Parser Resource
parseResourceAmount = Resource <$> (skipComments *> parseResource) <* whiteSpace <*> parseInt <* whiteSpace <*> parseInt <* optional newline

parseResourceList :: Parser ResourceList
parseResourceList = ResourceList <$> manyTill parseResourceAmount (try eof)

parseResource :: Parser ResourceType
parseResource = fromLetter <$> characterChar

fromLetter :: Char -> ResourceType
fromLetter = \case
    'A' -> Adder
    'S' -> Substracter
    'M' -> Multiplier
    'C' -> Comparator
    _   -> error "Failing parsing character"

parseNode :: Parser Node
parseNode = Node <$> (skipComments *> parseResource)
                 <* whiteSpace
                 <*> parseInt
                 <* whiteSpace
                 <*> parseInt
                 <* whiteSpace
                 <*> parseInt
                 <* whiteSpace
                 <*> optional parseInt
                 <* whiteSpace <* optional newline

parseSchedule' :: Parser Schedule
parseSchedule' = Schedule <$> parseAsap 
                          <* skipAlapHeader
                          <*> parseAlap
                          <* skipResourceListHeader
                          <*> parseResourceList

parseSchedule :: Parser Schedule
parseSchedule = skipAsapHeader *> parseSchedule'



-- parse :: IO ()
-- parse = do
--   content <- maybe [] identity <$> parseFromFile parseResults "results.txt"
--   forM_ content $ \(f, optimum)  -> do
--     whenM (doesFileExist $ toS f) $ do
--       result <- maybe 0 identity <$> parseFromFile parseOutput (toS f)
--       if result == optimum
--         then print (f <> ": OK")
--         else print (f <> ": ERROR --> OPTIMUM " <> show optimum <> " ---> MY SOLUTION " <> show result)
