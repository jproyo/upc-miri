module Data.Tree.TST
  ( TST
  , search
  , insert
  , fromList
  , countWords
  )
  where

----------------------------------------------------------------------------------

import           Protolude hiding (empty)

----------------------------------------------------------------------------------

data TST
  = Node Char TST TST TST
  | Word TST
  | Empty
  deriving (Show, Generic, NFData)

search :: TST -> [Char] -> Bool
search Empty _ = False
search (Word _) [] = True
search (Node _ l _ _) [] = search l []
search (Word rest) xs = search rest xs
search (Node e l m r) xss@(x:xs)
  | e == x = search m xs
  | e > x = search l xss
  | otherwise = search r xss

insert :: TST -> [Char] -> TST
insert Empty [] = Word Empty
insert w@(Word _) [] = w
insert Empty xs = singleton xs
insert (Word rest) xs = Word $ insert rest xs
insert (Node ele l m r) [] = Node ele (insert l []) m r
insert (Node e l m r) xss@(x:xs)
  | e == x = Node e l (insert m xs) r
  | e > x = Node e (insert l xss) m r
  | otherwise = Node e l m (insert r xss)

singleton :: [Char] -> TST
singleton (x:xs) = Node x Empty (singleton xs) Empty
singleton []     = Word Empty

empty :: TST
empty = Empty

fromList :: [[Char]] -> TST
fromList = foldr (flip insert) empty

countWords :: TST -> Int
countWords Empty          = 0
countWords (Word rest)    = 1 + countWords rest
countWords (Node _ l m r) = countWords l + countWords m + countWords r
