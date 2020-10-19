module Data.Lang where

import Relude


data Language = Language 
    { language   :: Text
    , nVertices  :: Integer
    , nEdges     :: Integer
    , edges      :: [(Text, Text)]
    } 

    