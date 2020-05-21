module Main where

import           Experiments
import           Protolude

main :: IO ()
main = undefined
--main =
--  defaultMain
--    [ env setupEnvLogN $ \ ~(input, test) ->
--        bgroup
--          "Experiments Ternary Search Tree"
--          [ bgroup
--              "Searching"
--              [ runEmpiricalSearchLogN (input, test)
--              ]
--          ]
--    , env setupMap $ \ ~(input, test) ->
--        bgroup
--          "Experiments HashMap for comparing Searching"
--          [ bgroup
--              "Searching"
--              [ runEmpiricalSearchMap (input, test)
--              ]
--          ]
--    ]
