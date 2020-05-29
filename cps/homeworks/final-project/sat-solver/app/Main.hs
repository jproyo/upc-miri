module Main where

--import           Control.Monad
--import           Options.Applicative
import           Solver
import           Protolude

main :: IO ()
main = fromInput >>= putLText . show . rollMaxLength
