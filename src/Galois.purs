module Galois
  ( initLookups
  , gfMul
  , gfDiv
  , gfPow
  , gfInv
  , gfPolyScale
  , gfPolyAdd
  , gfPolyMul
  ) where

import Control.Monad.State (State, get)
import Data.Array (drop, foldr, length, replicate, snoc, take, updateAt, zipWith, (!!))
import Data.Int.Bits (xor, shl)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), swap)
import Effect.Exception.Unsafe (unsafeThrow)
import Prelude

-- Lookups = (Exp, Log)
type Lookups
  = Tuple (Array Int) (Array Int)

type StateL
  = State Lookups

type Poly
  = Array Int

primitive :: Int
primitive = 0x11d

initLookups :: Lookups
initLookups = initExpLog 0 1 $ Tuple [] $ replicate 256 0

initExpLog :: Int -> Int -> Lookups -> Lookups
initExpLog 0 i (Tuple exp log) = initExpLog i 2 $ Tuple [ i ] $ updateLogAt 2 i log

initExpLog 255 x lkps = extendExp lkps

initExpLog i x (Tuple exp log)
  | x > 255 = initExpLog (i + 1) (shl x' 1) $ Tuple (snoc exp x') $ updateLogAt x' i log
    where
    x' = xor x primitive
  | otherwise = initExpLog (i + 1) (shl x 1) $ Tuple (snoc exp x) $ updateLogAt x i log

extendExp :: Lookups -> Lookups
extendExp (Tuple exp log) = swap $ Tuple log $ exp <> exp <> take 2 exp

gfMul :: Int -> Int -> StateL Int
gfMul x y = do
  (Tuple exp log) <- get
  pure
    $ case x == 0 || y == 0 of
        true -> 0
        _ -> exp !!! (log !!! x + log !!! y)

gfDiv :: Int -> Int -> StateL Int
gfDiv x y = do
  (Tuple exp log) <- get
  let
    z
      | x == 0 = 0
      | y == 0 = unsafeThrow $ "gfDiv: division by zero with x=" <> show x
      | otherwise = (!!!) exp $ mod (log !!! x + 255 - log !!! y) 255
  pure z

gfPow :: Int -> Int -> StateL Int
gfPow x y = do
  (Tuple exp log) <- get
  pure $ exp !!! mod (log !!! x * y) 255

gfInv :: Int -> StateL Int
gfInv x = do
  (Tuple exp log) <- get
  pure $ exp !!! (255 - log !!! x)

gfPolyScale :: Poly -> Int -> StateL Poly
gfPolyScale p x = for p $ gfMul x

gfPolyAdd :: Poly -> Poly -> Poly
gfPolyAdd p1 p2 =
  let
    d = length p1 - length p2

    absd = abs d
  in
    case d > 0 of
      true -> take absd p1 <> zipWith xor (drop absd p1) p2
      _ -> take absd p2 <> zipWith xor (drop absd p2) p1

gfPolyMul :: Poly -> Poly -> StateL Poly
gfPolyMul p1 p2 = do
  let
    zeros = replicate (length p1 + length p2 - 1) 0
  v <-
    forWithIndex p1
      $ \i x -> do
          a <- for p2 $ gfMul x
          pure $ take i zeros <> a <> drop i zeros
  pure $ foldr (\x a -> zipWith xor a x) zeros v

-----------------
----  Utils  ----
-----------------
--
-- Replicate crash on illegal use in Haskell
--
updateLogAt :: Int -> Int -> Array Int -> Array Int
updateLogAt ind val log = case updateAt ind val log of
  Just log' -> log'
  Nothing -> unsafeThrow $ "updateLogAt failed with index " <> show ind

unsafeIndex :: Array Int -> Int -> Int
unsafeIndex a i = case a !! i of
  Just v -> v
  Nothing -> unsafeThrow $ "unsafeIndex failed with index " <> show i

infixl 8 unsafeIndex as !!!
