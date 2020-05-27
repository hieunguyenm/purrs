module RS (encodeMsg, generator) where

import Prelude
import Data.Array (length, replicate)
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Galois (Poly, StateL, gfPolyDiv, gfPolyMul, gfPow)

generator :: Int -> StateL Poly
generator sym = genPoly sym 0 [ 1 ]

genPoly :: Int -> Int -> Poly -> StateL Poly
genPoly sym i p
  | sym == i = pure p
  | otherwise = do
    pow <- gfPow 2 i
    g <- gfPolyMul p [ 1, pow ]
    genPoly sym (i + 1) g

encodeMsg :: Array Int -> Int -> StateL (Array Int)
encodeMsg msg sym
  | length msg + sym > 255 =
    unsafeThrow
      $ "encodeMsg: message is too long with msg="
      <> show msg
      <> ", sym="
      <> show sym
  | otherwise = do
    g <- generator sym
    (Tuple _ r) <- gfPolyDiv (msg <> replicate (length g - 1) 0) g
    pure $ msg <> r
