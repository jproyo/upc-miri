module RandomBST
  ( insert
  , delete
  , empty
  , height
  , fromList
  )
where

import           System.Random
import           Prelude                 hiding ( null )


data Treap k p = Empty | Node (Treap k p) k p (Treap k p)
 deriving (Show, Read)

newtype RTreap g k p = RT (g, Treap k p)
 deriving (Show, Read)

treapEmpty :: Treap k p
treapEmpty = Empty

treapHeight :: Treap k p -> Int
treapHeight Empty = 0
treapHeight (Node left _ _ right) =
  1 + max (treapHeight left) (treapHeight right)

rotateLeft :: Treap k p -> Treap k p
rotateLeft (Node a k p (Node b1 k' p' b2)) = Node (Node a k p b1) k' p' b2
rotateLeft _                               = error "Wrong rotation (rotateLeft)"

rotateRight :: Treap k p -> Treap k p
rotateRight (Node (Node a1 k' p' a2) k p b) = Node a1 k' p' (Node a2 k p b)
rotateRight _ = error "Wrong rotation (rotateRight)"

treapInsert :: (Ord k, Ord p) => k -> p -> Treap k p -> Treap k p
treapInsert k p Empty                   = Node Empty k p Empty
treapInsert k p (Node left k' p' right) = case compare k k' of
  EQ -> Node left k' p' right -- Node is already there, ignore
  LT -> case Node (treapInsert k p left) k' p' right of
    t@(Node (Node _ _ p'' _) _ p''' _) ->
      if p''' > p'' then rotateRight t else t
    t -> t
  GT -> case Node left k' p' (treapInsert k p right) of
    t@(Node _ _ p''' (Node _ _ p'' _)) ->
      if p''' > p'' then rotateLeft t else t
    t -> t


treapDelete :: (Ord k, Ord p) => k -> Treap k p -> Treap k p
treapDelete = recDelete
 where
  recDelete _ Empty = error "Key does not exist in tree (treapDelete)"
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

empty :: RandomGen g => g -> RTreap g k p
empty g = RT (g, treapEmpty)

insert
  :: (RandomGen g, Ord k, Ord p, Num p, Random p)
  => k
  -> RTreap g k p
  -> RTreap g k p
insert k (RT (g, tr)) =
  let (p, g') = randomR (-2000000000, 2000000000) g
  in  RT (g', treapInsert k p tr)

delete :: (RandomGen g, Ord k, Ord p) => k -> RTreap g k p -> RTreap g k p
delete k (RT (g, tr)) = RT (g, treapDelete k tr)

height :: RTreap g k p -> Int
height (RT (_, treap)) = treapHeight treap

fromList
  :: (Ord a, Ord p, Num p, Random p, RandomGen g) => [a] -> g -> RTreap g a p
fromList xs g = foldl (flip insert) (empty g) xs


