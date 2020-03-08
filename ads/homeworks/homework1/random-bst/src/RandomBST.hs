{-# LANGUAGE RankNTypes #-}

{-
All this source code was taken and modified from here https://wiki.haskell.org/The_Monad.Reader/Issue4/On_Treaps_And_Randomization
-}
module RandomBST
  ( RTreap
  , insert
  , delete
  , empty
  , height
  , fromList
  , leafDepth
  , depth
  , nodes
  )
where

------------------------------------------------------------------------------------------------------------------------

import           System.Random
import           Prelude                 hiding ( null )

------------------------------------------------------------------------------------------------------------------------

data Treap k p = Empty | Node (Treap k p) k p (Treap k p)
 deriving (Show, Read)

newtype RTreap g k p = RT (g, Treap k p)
 deriving (Show, Read)

empty :: RandomGen g => g -> RTreap g k p
empty g = RT (g, tEmpty)

tEmpty :: Treap k p
tEmpty = Empty

tHeight :: Treap k p -> Int
tHeight Empty                 = 0
tHeight (Node left _ _ right) = 1 + max (tHeight left) (tHeight right)

tDepth :: Treap k p -> Int
tDepth tree = calc tree 0
 where
  calc Empty          _ = 0
  calc (Node l _ _ r) d = d + calc l (d + 1) + calc r (d + 1)

tNodes :: Treap k p -> Int
tNodes Empty            = 0
tNodes (Node le _ _ ri) = 1 + tNodes le + tNodes ri

tLeafDepth :: Treap k p -> [Int]
tLeafDepth = flip (heights 0) []
 where
  heights dth Empty accum = dth : accum
  heights dth (Node l _ _ r) accum =
    heights (dth + 1) l $ heights (dth + 1) r accum

rotateLeft :: Treap k p -> Treap k p
rotateLeft (Node a k p (Node b1 k' p' b2)) = Node (Node a k p b1) k' p' b2
rotateLeft _                               = error "Wrong rotation (rotateLeft)"

rotateRight :: Treap k p -> Treap k p
rotateRight (Node (Node a1 k' p' a2) k p b) = Node a1 k' p' (Node a2 k p b)
rotateRight _ = error "Wrong rotation (rotateRight)"

tInsert :: (Ord k, Ord p) => k -> p -> Treap k p -> Treap k p
tInsert k p Empty                     = Node Empty k p Empty
tInsert k p t@(Node left k' p' right) = case compare k k' of
  EQ -> t
  LT -> case Node (tInsert k p left) k' p' right of
    t'@(Node (Node _ _ p'' _) _ p''' _) ->
      if p''' > p'' then rotateRight t' else t'
    _ -> t
  GT -> case Node left k' p' (tInsert k p right) of
    t'@(Node _ _ p''' (Node _ _ p'' _)) ->
      if p''' > p'' then rotateLeft t' else t'
    _ -> t


tDelete :: (Show k, Ord k, Ord p) => k -> Treap k p -> Treap k p
tDelete key = recDelete key
 where
  recDelete _ Empty =
    error ("Key does not exist in tree (tDelete) - Key: " <> show key)
  recDelete k' t@(Node left k'' p right) = case compare k' k'' of
    LT -> Node (recDelete k' left) k'' p right
    GT -> Node left k'' p (recDelete k' right)
    EQ -> rootDelete t

  priorityCompare Empty          Node{}         = False
  priorityCompare Node{}         Empty          = True
  priorityCompare (Node _ _ x _) (Node _ _ y _) = x < y
  priorityCompare _              _              = False

  rootDelete Empty                    = Empty
  rootDelete (  Node Empty _ _ Empty) = Empty
  rootDelete t@(Node left  _ _ right) = if priorityCompare left right
    then
      let Node left' k p right' = rotateRight t
      in  Node left' k p (rootDelete right')
    else
      let Node left' k p right' = rotateLeft t
      in  Node (rootDelete left') k p right'


insert
  :: (RandomGen g, Ord k, Bounded p, Ord p, Num p, Random p)
  => k
  -> RTreap g k p
  -> RTreap g k p
insert k (RT (g, tr)) =
  let (p, g') = randomR (0, maxBound) g in RT (g', tInsert k p tr)

delete
  :: (Show k, RandomGen g, Ord k, Ord p) => k -> RTreap g k p -> RTreap g k p
delete k (RT (g, tr)) = RT (g, tDelete k tr)

applyToTreap :: (Treap k p -> r) -> RTreap g k p -> r
applyToTreap f (RT (_, treap)) = f treap

height :: RTreap g k p -> Int
height = applyToTreap tHeight

depth :: RTreap g k p -> Int
depth = applyToTreap tDepth

nodes :: RTreap g k p -> Int
nodes = applyToTreap tNodes

leafDepth :: RTreap g k p -> [Int]
leafDepth = applyToTreap tLeafDepth

fromList
  :: (Ord a, Ord p, Bounded p, Num p, Random p, RandomGen g)
  => [a]
  -> g
  -> RTreap g a p
fromList xs g = foldl (flip insert) (empty g) xs


