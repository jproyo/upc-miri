module Output.Report where

import Data.Lang
import Data.Text as T
import Relude as R
import Text.Printf as P

data Table1 = Table1
    { l :: Text
    , n :: Integer
    , e :: Int
    , k :: Double
    , d :: Double
    }

data Table2 = Table2
    { l2 :: Text
    , c2 :: Float
    }

printTable1 :: [Table1] -> IO ()
printTable1 = mapM_ printRowTable1

printRowTable1 :: Table1 -> IO ()
printRowTable1 Table1{..} = P.printf "%s & %d & %d & %.7f & %.7f \n" l n e k d

printTable2 :: [Table2] -> IO ()
printTable2 = mapM_ printRowTable2

printRowTable2 :: Table2 -> IO ()
printRowTable2 Table2{..} = P.printf "%s & %.7f & & \\\\ \n" l2 c2


table1 :: [Language] -> [Table1]
table1 = R.map toTable1

toTable1 :: Language -> Table1
toTable1 lang = Table1 { l = language lang
                       , n = nVertices lang
                       , e = nEdges lang
                       , k = kCalc lang
                       , d =  delta lang
                       }