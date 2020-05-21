module Data.Zipper.Random where

import           Control.Monad.Random hiding (fromList)
import           Data.Bits
import           Data.Foldable        (toList)
import           Prelude              (Show, error, showParen, showString, shows, showsPrec)
import           Protolude            hiding (fold)

type NLev = Int

type Cnt = Int

data Dir
  = L
  | R
  deriving (Eq, Show, Ord)

data Tree a
  = Empty
  | Leaf a
  | Bin !NLev !Cnt !(Tree a) !(Tree a)
  deriving (Functor, Foldable, Traversable)

instance Monoid (Tree a) where
  mempty = Empty

instance Semigroup (Tree a) where
  (<>) = joinSides

data TList a
  = Nil
  | Cons a !(TList a)
  | Level !NLev !(TList a)
  | LTree !(Tree a) !(TList a)
  deriving (Functor, Foldable, Traversable)

type Raz a = (TList a, a, TList a)

instance Eq a => Eq (Tree a) where
  (==) = (==) `on` toList

instance Ord a => Ord (Tree a) where
  compare = compare `on` toList

instance Show a => Show (Tree a) where
  showsPrec d t = showParen (d > 10) $ showString "fromList " . shows (toList t)

randomLevel :: MonadRandom m => m NLev
randomLevel = fmap (\n -> countTrailingZeros (n :: Word)) getRandom
{-# INLINE randomLevel #-}
{-# SPECIALISE randomLevel :: IO NLev #-}

singleton :: a -> Raz a
singleton a = (Nil, a, Nil)

empty :: MonadRandom m => a -> m (Raz a)
empty a = randomLevel <&> \lv -> (Level lv ((Cons a) Nil), a, Nil)
{-# INLINE empty #-}
{-# SPECIALISE Data.Zipper.Random.empty :: Int -> IO (Raz Int) #-}

size :: Tree a -> Cnt
size Empty           = 0
size (Leaf _)        = 1
size (Bin _ cnt _ _) = cnt

trim :: Dir -> TList a -> TList a
trim d (LTree t r) = trim' d t r
trim _ tl          = tl

trim' :: Dir -> Tree a -> TList a -> TList a
trim' _ Empty _ = error "Malformed Tree"
trim' _ (Leaf a) l = Cons a l
trim' d (Bin lv _ l r) l2
  | d == L = trim R (Level lv (LTree l l2))
  | otherwise = trim L (Level lv (LTree r l2))

viewC :: Raz a -> a
viewC (_, x, _) = x

view :: Dir -> Raz a -> a
view d (l, _, r)
  | d == L = viewList d l
  | otherwise = viewList d r

viewList :: Dir -> TList a -> a
viewList _ Nil         = error "Cannot view here"
viewList _ (Cons x _)  = x
viewList d (Level _ l) = viewList d l
viewList d t           = viewList d (trim d t)

replaceC :: a -> Raz a -> Raz a
replaceC x (l, _, r) = (l, x, r)

alter :: Dir -> a -> Raz a -> Raz a
alter d x (l, e, r)
  | d == L = (alterList x d l, e, r)
  | otherwise = (l, e, alterList x d r)

alterList :: a -> Dir -> TList a -> TList a
alterList _ _ Nil          = error "Cannot alter here"
alterList x _ (Cons _ l)   = Cons x l
alterList x d (Level lv l) = Level lv (alterList x d l)
alterList x d t            = alterList x d (trim d t)

insert :: MonadRandom m => Dir -> a -> Raz a -> m (Raz a)
insert d x (l, e, r) =
  randomLevel <&> \lv ->
    case d of
      L -> (Level lv (Cons x l), e, r)
      R -> (l, e, Level lv (Cons x r))
{-# INLINE insert #-}
{-# SPECIALISE insert :: Dir -> Int -> Raz Int -> IO (Raz Int) #-}


remove :: Dir -> Raz a -> Raz a
remove d (l, e, r)
  | d == L = (removeList d l, e, r)
  | otherwise = (l, e, removeList d r)

removeList :: Dir -> TList a -> TList a
removeList _ Nil          = error "Nothing to remove here"
removeList _ (Cons _ l)   = l
removeList d (Level _ l') = removeList d l'
removeList d t            = removeList d (trim d t)

move :: Dir -> Raz a -> Raz a
move d (l, e, r)
  | d == L = moveOver d l (Cons e r)
  | otherwise = moveOver d r (Cons e l)
{-# INLINE move #-}

moveOver :: Dir -> TList a -> TList a -> Raz a
moveOver _ Nil _           = error "Nothing to move over"
moveOver _ (Cons e l) r    = (l, e, r)
moveOver d (Level lv l') r = moveOver d l' (Level lv r)
moveOver d t r             = moveOver d (trim d t) r
{-# INLINE moveOver #-}

focus :: Int -> Tree a -> Raz a
focus p t
  | p < 0 || p >= size t = error "focus out of bounds"
focus p t = focus' p Nil Nil t
{-# INLINE focus #-}

focus' :: Int -> TList a -> TList a -> Tree a -> Raz a
focus' _ _ _ Empty = error "internal Empty"
focus' _ !l !r (Leaf a) = (l, a, r) -- p == 0
focus' p !l !r (Bin lv _ bl br)
  | p < c = focus' p l (Level lv (LTree br r)) bl
  | otherwise = focus' (p - c) (Level lv (LTree bl l)) r br
  where
    c = size bl
{-# INLINE focus' #-}

focusL :: Tree a -> Raz a
focusL = focusL' Nil
{-# INLINE focusL #-}

focusL' :: TList a -> Tree a -> Raz a
focusL' _ Empty            = error "internal Empty"
focusL' !r (Leaf a)         = (Nil, a, r)
focusL' !r (Bin lv _ bl br) = focusL' (Level lv (LTree br r)) bl
{-# INLINE focusL' #-}

focusR :: Tree a -> Raz a
focusR = focusR' Nil
{-# INLINE focusR #-}

focusR' :: TList a -> Tree a -> Raz a
focusR' _ Empty            = error "internal Empty"
focusR' !l (Leaf a)         = (l, a, Nil)
focusR' !l (Bin lv _ bl br) = focusR' (Level lv (LTree bl l)) br
{-# INLINE focusR' #-}

joinSides :: Tree a -> Tree a -> Tree a
joinSides t1 t2 =
  case (t1, t2) of
    (Empty, _) -> t2
    (_, Empty) -> t1
    (Leaf _, Leaf _) -> error "leaf-leaf: full trees shouldn't be joined"
    (Leaf _, Bin lv _ l r) -> Bin lv tot (joinSides t1 l) r
    (Bin lv _ l r, Leaf _) -> Bin lv tot l (joinSides r t2)
    (Bin lv1 _ l1 r1, Bin lv2 _ l2 r2)
      | lv1 >= lv2 -> Bin lv1 tot l1 (joinSides r1 t2)
      | otherwise -> Bin lv2 tot (joinSides t1 l2) r2
  where
    tot = size t1 + size t2
{-# INLINE joinSides #-}

headAsTree :: TList a -> Tree a
headAsTree Nil         = Empty
headAsTree (Cons x _)  = Leaf x
headAsTree (Level l _) = Bin l 0 Empty Empty
headAsTree (LTree t _) = t
{-# INLINE headAsTree #-}

tail :: TList a -> TList a
tail Nil         = Nil
tail (Cons _ r)  = r
tail (Level _ r) = r
tail (LTree _ r) = r
{-# INLINE tail #-}

grow :: Dir -> TList a -> Tree a
grow d t = grow' d (headAsTree t) (tail t)
{-# INLINE grow #-}

grow' :: Dir -> Tree a -> TList a -> Tree a
grow' _ h Nil = h
grow' d h t =
  case d of
    L -> grow' d (joinSides h' h) t'
    R -> grow' d (joinSides h h') t'
  where
    h' = headAsTree t
    t' = tail t
{-# INLINE grow' #-}

unfocus :: Raz a -> Tree a
unfocus (l, e, r) = let !gr = grow R r
                        !gl = grow L l
                     in joinSides gl . joinSides (Leaf e) $ gr
{-# INLINE unfocus #-}

fromList :: MonadRandom m => [a] -> m (Tree a)
fromList []     = return Empty
fromList (a:as) = fromList' (LTree (Leaf a) Nil) as

fromList' :: MonadRandom m => TList a -> [a] -> m (Tree a)
fromList' !ls [] = return (fold ls)
fromList' !ls (a:as) =
  randomLevel >>= \lv -> fromList' (LTree (Leaf a) (push lv ls)) as

push :: NLev -> TList a -> TList a
push lv (LTree t (Level lv' (LTree t' ls)))
  | lv > lv' = push lv (LTree (bin lv' t' t) ls)
push lv ls = Level lv ls

fold :: TList a -> Tree a
fold (LTree t (Level lv (LTree t' ls))) = fold (LTree (bin lv t' t) ls)
fold (LTree t _)                        = t
fold _                                  = error "internal error"

bin :: NLev -> Tree a -> Tree a -> Tree a
bin lv l r = Bin lv tot l r
  where
    tot = size l + size r

insertAt' :: MonadRandom m => Dir -> Int -> a -> Tree a -> m (Tree a)
insertAt' d i a = fmap unfocus . insert d a . focus i
{-# INLINE insertAt' #-}
{-# SPECIALISE insertAt' :: Dir -> Int -> Int -> Tree Int -> IO (Tree Int) #-}


showRaz :: Show a => Raz a -> Text
showRaz (l', a, r') =
  let l = halfToList L l' mempty
      r = reverse (halfToList R r' mempty)
   in (foldl stringify `flip` r) . (\as -> as <> ":" <> show a <> ":,") $
      foldl stringify mempty l
  where
    stringify as e = as <> show e <> ", "

treeToList :: Dir -> Tree a -> [a] -> [a]
treeToList _ Empty         = identity
treeToList _ (Leaf a)      = (a :)
treeToList L (Bin _ _ l r) = treeToList L l . treeToList L r
treeToList R (Bin _ _ l r) = treeToList R r . treeToList R l

halfToList :: Dir -> TList a -> [a] -> [a]
halfToList _ Nil            = identity
halfToList d (Cons a rest)  = halfToList d rest . (a :)
halfToList d (Level _ rest) = halfToList d rest
halfToList d (LTree t rest) = halfToList d rest . treeToList d t

showTree :: Show a => Tree a -> Text
showTree Empty = "Empty"
showTree (Leaf a) = show a
showTree (Bin lv _ l r) =
  mconcat ["(", showTree l, ") ", show lv, " (", showTree r, ")"]

lengthR :: Raz a -> Int
lengthR = size . unfocus

fromListToRaz :: MonadRandom m => [a] -> m (Raz a)
fromListToRaz (x:xs) = insertL' xs 0 (singleton x)
fromListToRaz []     = error "Cannot build an empty list"
{-# INLINE fromListToRaz #-}
{-# SPECIALISE fromListToRaz :: [Int] -> IO (Raz Int) #-}


insertL' :: MonadRandom m => [a] -> Int -> Raz a -> m (Raz a)
insertL' [] _ !r = return r
insertL' (x:xs) sz !r = do
  p <- getRandomR (0, sz)
  r' <- (insert L x . focus p . unfocus) $ r
  insertL' xs (sz + 1) r'
{-# INLINE insertL' #-}
{-# SPECIALISE insertL' :: [Int] -> Int -> Raz Int -> IO (Raz Int) #-}


