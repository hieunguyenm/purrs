module Test.Galois where

import Control.Monad.State (evalState)
import Data.Array (length)
import Data.Tuple (fst, snd)
import Galois (gfDiv, gfInv, gfMul, gfPolyAdd, gfPolyEval, gfPolyMul, gfPolyScale, gfPow, initLookups)
import Prelude (Unit, discard, flip, bind, pure, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Galois" do
    let
      lkps = initLookups
    it "generates lookup lists of correct lengths" do
      shouldEqual 512 $ length $ fst lkps
      shouldEqual 256 $ length $ snd lkps
    it "implements Galois multiplication correctly" do
      a1 <- pure $ flip evalState lkps $ gfMul 0 20
      shouldEqual 0 a1
      a2 <- pure $ flip evalState lkps $ gfMul 20 0
      shouldEqual 0 a2
      a3 <- pure $ flip evalState lkps $ gfMul 137 42
      shouldEqual 195 a3
      a4 <- pure $ flip evalState lkps $ gfMul 4 8
      shouldEqual 32 a4
      a5 <- pure $ flip evalState lkps $ gfMul 8 32
      shouldEqual 29 a5
    it "implements Galois division correctly" do
      a1 <- pure $ flip evalState lkps $ gfDiv 0 20
      shouldEqual 0 a1
      a3 <- pure $ flip evalState lkps $ gfDiv 195 42
      shouldEqual 137 a3
      a4 <- pure $ flip evalState lkps $ gfDiv 32 4
      shouldEqual 8 a4
      a5 <- pure $ flip evalState lkps $ gfDiv 29 8
      shouldEqual 32 a5
    it "implements Galois power correctly" do
      a1 <- pure $ flip evalState lkps $ gfPow 4 4
      shouldEqual 29 a1
      a2 <- pure $ flip evalState lkps $ gfPow 4 3
      shouldEqual 64 a2
    it "implements Galois inverse correctly" do
      got1 <- pure $ flip evalState lkps $ gfInv 2
      wnt1 <- pure $ flip evalState lkps $ gfDiv 1 2
      shouldEqual got1 wnt1
      got2 <- pure $ flip evalState lkps $ gfInv 255
      wnt2 <- pure $ flip evalState lkps $ gfDiv 1 255
      shouldEqual got2 wnt2
      got3 <- pure $ flip evalState lkps $ gfInv 64
      wnt3 <- pure $ flip evalState lkps $ gfDiv 1 64
      shouldEqual got3 wnt3
    it "scales polynomials correctly" do
      a1 <- pure $ flip evalState lkps $ gfPolyScale [ 1, 2, 3, 4 ] 2
      shouldEqual [ 2, 4, 6, 8 ] a1
      a2 <- pure $ flip evalState lkps $ gfPolyScale [ 64, 92, 36, 7 ] 4
      shouldEqual [ 29, 109, 144, 28 ] a2
    it "adds polynomials correctly" do
      shouldEqual [ 1, 0, 1, 1, 0 ] $ gfPolyAdd [ 1, 0, 0, 1, 1 ] [ 1, 0, 1 ]
      shouldEqual [ 2, 47, 36, 26 ] $ gfPolyAdd [ 5, 32, 7, 25 ] [ 7, 15, 35, 3 ]
    it "multiplies polynomials correctly" do
      a1 <- pure $ flip evalState lkps $ gfPolyMul [ 1, 1, 1, 0, 1 ] [ 1, 0, 1 ]
      shouldEqual [ 1, 1, 0, 1, 0, 0, 1 ] a1
      a2 <- pure $ flip evalState lkps $ gfPolyMul [ 1, 1, 0, 0, 1 ] [ 1, 0, 1 ]
      shouldEqual [ 1, 1, 1, 1, 1, 0, 1 ] a2
    it "evaluates polynomials correctly" do
      a1 <- pure $ flip evalState lkps $ gfPolyEval [ 1, 15, 36, 78, 40 ] 3
      shouldEqual 10 a1
      a2 <- pure $ flip evalState lkps $ gfPolyEval [ 0x1, 0xF, 0x36, 0x78, 0x40 ] 1
      shouldEqual 0 a2
