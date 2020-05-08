module Main where

import           Criterion.Main
import           Experiments
import           Protolude

main :: IO ()
main =
  defaultMain
    [ env setupEnvLogN $ \ ~(input, test) ->
        bgroup
          "Experiments Ternary Search Tree"
          [ bgroup
              "Searching"
              [ runEmpiricalSearchLogN (input, test)
              ]
          ]
    ]
