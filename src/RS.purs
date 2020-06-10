module RS (calcSyndromes, check, encodeMsg, generator) where

import Prelude
import Data.Array ((..), length, replicate)
import Data.Foldable (maximum)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Galois (Poly, StateL, gfPolyDiv, gfPolyEval, gfPolyMul, gfPow)
import Partial.Unsafe (unsafeCrashWith)

generator :: Int -> StateL Poly
generator sym = genPoly sym 0 [ 1 ]

genPoly :: Int -> Int -> Poly -> StateL Poly
genPoly sym i p
  | sym == i = pure p
  | otherwise = do
    pow <- gfPow 2 i
    g <- gfPolyMul p [ 1, pow ]
    genPoly sym (i + 1) g

encodeMsg :: Poly -> Int -> StateL Poly
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

calcSyndromes :: Poly -> Int -> StateL Poly
calcSyndromes msg sym = foldWithIndexM f [] $ 0 .. (sym - 1)
  where
  f = \i a _ -> do
    p <- gfPow 2 i
    v <- gfPolyEval msg p
    pure $ a <> [ v ]

check :: Poly -> Int -> StateL Boolean
check msg sym = do
  p <- calcSyndromes msg sym
  pure
    $ case maximum p of
        Just 0 -> true
        Just _ -> false
        _ -> unsafeCrashWith $ "check: failed to find maximum with p=" <> show p
