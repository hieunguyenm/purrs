module Test.RS where

import Prelude
import Control.Monad.State (evalState)
import Data.Array (drop, replicate)
import Galois (initLookups)
import RS (check, encodeMsg, generator, syndrome)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "RS" do
    let
      lkps = initLookups

      plainMsg = [ 0x40, 0xd2, 0x75, 0x47, 0x76, 0x17, 0x32, 0x06, 0x27, 0x26, 0x96, 0xc6, 0xc6, 0x96, 0x70, 0xec ]

      encodedMsg = plainMsg <> [ 0xbc, 0x2a, 0x90, 0x13, 0x6b, 0xaf, 0xef, 0xfd, 0x4b, 0xe0 ]

      corruptMsg = [ 0 ] <> drop 1 encodedMsg

      sym = 10
    it "calculates correct generator polynomials" do
      a1 <- pure $ flip evalState lkps $ generator sym
      shouldEqual ([ 1, 216, 194, 159, 111, 199, 94, 95, 113, 157, 193 ]) a1
    it "encodes messages" do
      a1 <-
        pure $ flip evalState lkps $ encodeMsg plainMsg sym
      shouldEqual encodedMsg a1
    it "calculates correct syndrome polynomials" do
      a1 <- pure $ flip evalState lkps $ syndrome encodedMsg sym
      shouldEqual a1 $ replicate sym 0
      a2 <- pure $ flip evalState lkps $ syndrome corruptMsg sym
      shouldEqual [ 64, 192, 93, 231, 52, 92, 228, 49, 83, 245 ] a2
    it "checks for corrupt messages correctly" do
      a1 <- pure $ flip evalState lkps $ check encodedMsg sym
      shouldEqual true a1
      a2 <- pure $ flip evalState lkps $ check corruptMsg sym
      shouldEqual false a2
