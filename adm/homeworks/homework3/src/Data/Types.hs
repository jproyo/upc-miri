{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Types where

newtype Tuple15 a = Tuple15 (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  deriving newtype Show

toTuple15 :: Show a => [a] -> Tuple15 a
toTuple15 [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15] = Tuple15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15)
toTuple15 xs                             = error ("List must contain 15 elements"<>show xs)

toListTuple15 :: Tuple15 a -> [a]
toListTuple15 (Tuple15 (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15)) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15]

tuple15D :: [Tuple15 a -> a]
tuple15D = [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15]
  where
    d1  (Tuple15 (e, _, _, _, _, _, _, _, _, _,_,_,_,_,_)) = e
    d2  (Tuple15 (_, e, _, _, _, _, _, _, _, _,_,_,_,_,_)) = e
    d3  (Tuple15 (_, _, e, _, _, _, _, _, _, _,_,_,_,_,_)) = e
    d4  (Tuple15 (_, _, _, e, _, _, _, _, _, _,_,_,_,_,_)) = e
    d5  (Tuple15 (_, _, _, _, e, _, _, _, _, _,_,_,_,_,_)) = e
    d6  (Tuple15 (_, _, _, _, _, e, _, _, _, _,_,_,_,_,_)) = e
    d7  (Tuple15 (_, _, _, _, _, _, e, _, _, _,_,_,_,_,_)) = e
    d8  (Tuple15 (_, _, _, _, _, _, _, e, _, _,_,_,_,_,_)) = e
    d9  (Tuple15 (_, _, _, _, _, _, _, _, e, _,_,_,_,_,_)) = e
    d10 (Tuple15 (_, _, _, _, _, _, _, _, _, e,_,_,_,_,_)) = e
    d11 (Tuple15 (_, _, _, _, _, _, _, _, _, _,e,_,_,_,_)) = e
    d12 (Tuple15 (_, _, _, _, _, _, _, _, _, _,_,e,_,_,_)) = e
    d13 (Tuple15 (_, _, _, _, _, _, _, _, _, _,_,_,e,_,_)) = e
    d14 (Tuple15 (_, _, _, _, _, _, _, _, _, _,_,_,_,e,_)) = e
    d15 (Tuple15 (_, _, _, _, _, _, _, _, _, _,_,_,_,_,e)) = e


