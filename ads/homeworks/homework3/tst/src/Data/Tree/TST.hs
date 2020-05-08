module Data.Tree.TST where

import           Protolude

data TST
  = Node Char TST TST TST
  | Empty
  deriving (Show)

search :: TST -> [Char] -> Bool
search Empty [] = True
search Empty (_:_) = False
search (Node _ _ _ _) [] = False
search (Node e l m r) (x:xs)
  | e == x = search m xs
  | e > x = search l xs
  | otherwise = search r xs

insert :: TST -> [Char] -> TST
insert Empty (x:xs) = insert (Node x Empty Empty Empty) xs
insert ts [] = ts
insert (Node e l m r) w@(x:xs)
  | x < e = Node e (insert l w) m r
  | x > e = Node e l m (insert r w)
  | otherwise = Node e l (insert m xs) r
