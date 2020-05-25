module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Galois (initLookups)

main :: Effect Unit
main = do
  log $ show initLookups
