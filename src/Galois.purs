module Galois
  ( StateL
  , Lookups
  , Poly
  , initLookups
  , gfMul
  , gfDiv
  , gfPow
  , gfInv
  , gfPolyScale
  , gfPolyAdd
  , gfPolyMul
  , gfPolyEval
  , gfPolyDiv
  ) where

import Prelude
import Control.Monad.State (State, get)
import Data.Array
  ( drop
  , dropEnd
  , length
  , replicate
  , snoc
  , take
  , takeEnd
  , uncons
  , updateAt
  , zipWith
  , (!!)
  )
import Data.Foldable (foldM, foldr)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Int.Bits (xor, shl)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), swap)
import Effect.Exception.Unsafe (unsafeThrow)

-- (Tuple exp log)
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
initExpLog 0 i (Tuple exp log) =
  initExpLog
    i
    2
    $ Tuple [ i ]
    $ updateLogAt 2 i log

initExpLog 255 x lkps = extendExp lkps

initExpLog i x (Tuple exp log)
  | x > 255 =
    initExpLog
      (i + 1)
      (shl x' 1)
      $ Tuple (snoc exp x')
      $ updateLogAt x' i log
    where
    x' = xor x primitive
  | otherwise =
    initExpLog
      (i + 1)
      (shl x 1)
      $ Tuple (snoc exp x)
      $ updateLogAt x i log

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

gfPolyEval :: Poly -> Int -> StateL Int
gfPolyEval p i = case uncons p of
  Just { head: x, tail: xs } ->
    foldM
      ( \a c -> do
          v <- gfMul a i
          pure $ xor v c
      )
      x
      xs
  Nothing ->
    unsafeThrow
      $ "gfPolyEval: failed to uncons with p="
      <> show p
      <> ", i="
      <> show i

gfPolyDiv :: Poly -> Poly -> StateL (Tuple Poly Poly)
gfPolyDiv dend dsor = do
  let
    sep = length dsor - 1

    zeros = replicate (length dend) 0
  res <-
    foldWithIndexM
      ( \i a1 _ ->
          let
            c = a1 !!! i
          in
            case c == 0 of
              true -> pure a1
              _ ->
                foldWithIndexM
                  ( \j a2 d -> case d == 0 of
                      true -> pure a2
                      _ -> do
                        v <- gfMul d c
                        pure $ zipWith xor a2
                          $ take (i + j + 1) zeros
                          <> [ v ]
                          <> drop (i + j + 2) zeros
                  )
                  a1
                  $ drop 1 dsor
      )
      dend
      $ take (length dend - length dsor + 1) dend
  pure $ Tuple (dropEnd sep res) $ takeEnd sep res

--
-- Utils to replicate crash on illegal use in Haskell
--
updateLogAt :: Int -> Int -> Array Int -> Array Int
updateLogAt ind val log = case updateAt ind val log of
  Just log' -> log'
  Nothing -> unsafeThrow $ "updateLogAt: failed with index " <> show ind

unsafeIndex :: Array Int -> Int -> Int
unsafeIndex a i = case a !! i of
  Just v -> v
  Nothing -> unsafeThrow $ "unsafeIndex: failed with index " <> show i

infixl 8 unsafeIndex as !!!
