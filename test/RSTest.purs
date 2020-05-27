module Test.RS where

import Prelude
import Control.Monad.State (evalState)
import Galois (initLookups)
import RS (encodeMsg, generator)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "RS" do
    let
      lkps = initLookups
    it "calculates correct generator polynomials" do
      a1 <- pure $ flip evalState lkps $ generator 10
      shouldEqual [ 1, 216, 194, 159, 111, 199, 94, 95, 113, 157, 193 ] a1
    it "encodes messages correctly" do
      a1 <-
        pure $ flip evalState lkps
          $ encodeMsg [ 0x40, 0xd2, 0x75, 0x47, 0x76, 0x17, 0x32, 0x06, 0x27, 0x26, 0x96, 0xc6, 0xc6, 0x96, 0x70, 0xec ] 10
      shouldEqual [ 0x40, 0xd2, 0x75, 0x47, 0x76, 0x17, 0x32, 0x6, 0x27, 0x26, 0x96, 0xc6, 0xc6, 0x96, 0x70, 0xec, 0xbc, 0x2a, 0x90, 0x13, 0x6b, 0xaf, 0xef, 0xfd, 0x4b, 0xe0 ] a1
