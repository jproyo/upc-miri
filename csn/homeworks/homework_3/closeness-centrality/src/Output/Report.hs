module Output.Report where

import Data.Lang
import Data.Text as T
import Relude as R

table1 :: [Language] -> Text
table1 = T.intercalate "\\\\ \n" . R.map toTable1

toTable1 :: Language -> Text
toTable1 Language {..} = T.intercalate " & " [language, show nVertices, show nEdges, show (2*nEdges `div` nVertices), show (2*nEdges `div` (nVertices*(nVertices-1)))]