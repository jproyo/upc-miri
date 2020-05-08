module RanGen where

import           Control.Monad
import           System.IO
import           System.Random

rollDNADice :: RandomGen g => g -> (Char, g)
rollDNADice gen = let (a, b) = randomR (1,4) gen
                   in (toGene a, b)

toGene :: Int -> Char
toGene 1 = 'A'
toGene 2 = 'T'
toGene 3 = 'G'
toGene 4 = 'C'

main :: IO ()
main = do
  let outputFile = "random.fna"
  withFile outputFile WriteMode $ \h -> do
    replicateM_ 122300000 $ do
      gen <- newStdGen
      let (_, l) = foldr (\_ (g, acc) -> let (a, b) = rollDNADice g in (b,a:acc)) (gen, []) [1..80]
      hPutStrLn h l
