module Main where

import Test.QuickCheck
import Control.Monad
import Criterion.Main

data KMP a =
  KMP
    { done :: Int
    , next :: (a -> KMP a)
    }

makeTable :: Eq a => [a] -> KMP a
makeTable xs = table
   where table = makeTable' xs (const table)

makeTable' []     failure = KMP 1 failure
makeTable' (x:xs) failure = KMP 0 test
   where  test  c = if c == x then success else failure c
          success = makeTable' xs (next (failure x))

matches :: Eq a => [a] -> [a] -> Int
matches as bs = match (makeTable as) bs
   where  match table []     = done table
          match table (b:bs) = done table + match (next table b) bs

toPair :: [String] -> (String, String)
toPair [s, t] = (s, t)
toPair _      = error "Wrong input"

solveMatching :: String -> String
solveMatching = unlines . fmap (show . uncurry solve . toPair . words) . lines

solve :: String -> String -> Int
solve s t = matches t . take (((length s)*2)-1) . concat . take 2 $ repeat s

main :: IO ()
main = quickCheck prop_matches

prop_matches :: Property
prop_matches = forAll csString $ \(as, bs) -> (as `matches` bs) == (bs `matches` as)

csString :: Gen (String, String)
csString =
  (,) <$> replicateM 100000 (elements ['a'..'z']) <*> replicateM 100000 (elements ['a'..'z'])

