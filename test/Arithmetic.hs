{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.Data
import Data.Function ((&))
import Data.Functor.Compose
import Data.Functor.HyFM.Fusion
import Data.Functor.HyFM.Hylos
import Data.Functor.HyFM.Zoo
import Data.Functor.Identity
import Data.Int
import Data.Vector ((!))
import Language.Haskell.TH (ExpQ, runQ)

-- Target of
--   (1) power function (and its variant)
--   (2) dot product of vectors
--   (3) polynomial
data Exp v x = Lit Int | Add x x | Mul x x | Var v deriving Functor
data P     x = St  Int | Dy  x                     deriving Functor

liftP :: (Exp v x -> x) -> P x -> x
liftP φ p = case p of St n -> φ (Lit n); Dy x -> x

observeExpQ :: P ExpQ -> ExpQ
observeExpQ = \case St n -> [| n |]; Dy e -> e
{-# INLINE observeExpQ #-}

observeInt :: P Int -> Int
observeInt = \case St _ -> 0; Dy m -> m
{-# INLINE observeInt #-}

arithRed :: Hylo (BAlg P (Exp v)) (Exp v) (Exp v) Fix
arithRed = bcataF τ
  where τ φ t = case t of
          Lit n -> St n
          Var v -> Dy (φ (Var v))
          Add (St m) (St n) -> St (m + n)
          Add (St 0) (Dy b) -> Dy b
          Add (Dy a) (St 0) -> Dy a
          Add     a      b  -> Dy (φ (Add (liftP φ a) (liftP φ b)))
          Mul (St m) (St n) -> St (m * n)
          Mul (St 0) (Dy _) -> St 0
          Mul (Dy _) (St 0) -> St 0
          Mul (St 1) (Dy b) -> Dy b
          Mul (Dy a) (St 1) -> Dy a
          Mul     a      b  -> Dy (φ (Mul (liftP φ a) (liftP φ b)))
        {-# INLINE τ #-}
{-# INLINE arithRed #-}

compile :: Hylo (Alg ExpQ) (Exp ExpQ) (Exp ExpQ) Fix
compile = cata φ
  where φ t = case t of
          Var v   -> v
          Lit n   -> [|    n    |]
          Add a b -> [| $a + $b |]
          Mul a b -> [| $a * $b |]
        {-# INLINE φ #-}
{-# INLINE compile #-}

count :: Hylo (Alg Int) (Exp a) (Exp a) Fix
count = cata φ
  where φ t = case t of
          Var _  -> 0
          Lit _   -> 0
          Add a b -> 1 + a + b
          Mul a b -> 1 + a + b
        {-# INLINE φ #-}
{-# INLINE count #-}

-- Power function

data Nat x = O | S x deriving Functor

toNat :: Hylo Fix Nat Nat (Coalg Int)
toNat = ana ψ
  where ψ = \case 0 -> O; n -> S (n - 1)
        {-# INLINE ψ #-}
{-# INLINE toNat #-}

------ Power ------

power :: v -> Hylo (BAlg Identity (Exp v)) Nat Nat Fix
power x = bcata τ
  where τ φ t = case t of
          O -> φ (Lit 1)
          S m -> φ (Mul (φ (Var x)) m)
        {-# INLINE τ #-}
{-# INLINE power #-}

fastPowE :: ExpQ -> Int -> ExpQ
fastPowE v n = runHylo (compile ∘ arithRed @ExpQ ∘ power @ExpQ v ∘ toNat) n
  & getCompose & runIdentity & observeExpQ

slowPowE :: ExpQ -> Int -> ExpQ
slowPowE v n =
  let !t₁ = runHylo toNat n
      !t₂ = runHylo (power v) t₁
      !t₃ = runHylo arithRed <$> t₂
      !t₄ = (fmap $ fmap $ runHylo compile) t₃
  in t₄ & runIdentity & observeExpQ

fastPowC :: Int -> Int
fastPowC n = runHylo (count @() ∘ arithRed @() ∘ power () ∘ toNat) n
  & getCompose & runIdentity & observeInt

slowPowC :: Int -> Int
slowPowC n =
  let t₁ = runHylo toNat n
      t₂ = runHylo (power ()) t₁
      t₃ = runHylo arithRed <$> t₂
      t₄ = (fmap $ fmap $ runHylo count) t₃
  in t₄ & runIdentity & observeInt

------ Polynomial ------

data Nonempty a x = Last a | Rest a x deriving Functor

toNonempty :: Hylo Fix (Nonempty Int) (Nonempty Int) (Coalg [Int])
toNonempty = ana ψ
  where ψ t = case t of
          [x]  -> Last x
          x:xs -> Rest x xs
          []   -> Last 0
        {-# INLINE ψ #-}
{-# INLINE toNonempty #-}

poly :: forall v. v
     -> Hylo (BAlg Identity (Exp v)) (Nonempty Int) (Nonempty Int) Fix
poly v = bcata τ
  where τ φ t = case t of
          Last c -> φ (Lit c)
          Rest c x -> φ (Add (φ (Mul x (φ (Var v)))) (φ (Lit c)))
        {-# INLINE τ #-}
{-# INLINE poly #-}

fastPolyC :: [Int] -> Int
fastPolyC t = runHylo (count @() ∘ (arithRed @() ∘ (poly () ∘ toNonempty))) t
  & getCompose & runIdentity & observeInt

slowPolyC :: [Int] -> Int
slowPolyC t =
  let t₁ = runHylo toNonempty t
      t₂ = runHylo (poly ()) t₁
      t₃ = runHylo arithRed <$> t₂
      t₄ = (fmap $ fmap $ runHylo count) t₃
  in t₄ & runIdentity & observeInt

fastPolyE :: ExpQ -> [Int] -> ExpQ
fastPolyE v t = runHylo (compile ∘ arithRed @ExpQ ∘ poly v ∘ toNonempty) t
  & getCompose & runIdentity & observeExpQ

slowPolyE :: ExpQ -> [Int] -> ExpQ
slowPolyE v t =
  let t₁ = runHylo toNonempty t
      t₂ = runHylo (poly v) t₁
      t₃ = runHylo arithRed <$> t₂
      t₄ = (fmap $ fmap $ runHylo compile) t₃
  in t₄ & runIdentity & observeExpQ

------ dot product ------

data List a x = Nil | Cons a x deriving Functor

toList :: Hylo Fix (List (Int, a)) (List (Int, a)) (Coalg (Int, [a]))
toList = ana ψ
  where ψ = \case (_, []) -> Nil; (n, x : xs) -> Cons (n, x) (n + 1, xs)
        {-# INLINE ψ #-}
{-# INLINE toList #-}

dot :: ExpQ
    -> Hylo (BAlg Identity (Exp ExpQ)) (List (Int, Int)) (List (Int, Int)) Fix
dot v = bcata τ
  where τ φ t = case t of
          Nil -> φ (Lit 0)
          Cons (idx, c) x ->
            φ (Add (φ (Mul (φ (Lit c)) (φ (Var [| $v ! idx|])))) x)
        {-# INLINE τ #-}
{-# INLINE dot #-}

fastDotE :: ExpQ -> [Int] -> ExpQ
fastDotE v t = runHylo (compile ∘ arithRed @ExpQ ∘ dot v ∘ toList @Int) (0, t)
  & getCompose & runIdentity & observeExpQ

slowDotE :: ExpQ -> [Int] -> ExpQ
slowDotE v t =
  let t₁ = runHylo toList (0, t)
      t₂ = runHylo (dot v) t₁
      t₃ = runHylo arithRed <$> t₂
      t₄ = (fmap $ fmap $ runHylo compile) t₃
  in t₄ & runIdentity & observeExpQ

fastDotC :: [Int] -> Int
fastDotC t = runHylo (count @ExpQ ∘ arithRed @ExpQ ∘ dot [| undefined |] ∘ toList @Int) (0, t)
  & getCompose & runIdentity & observeInt

slowDotC :: [Int] -> Int
slowDotC t =
  let t₁ = runHylo toList (0, t)
      t₂ = runHylo (dot [|undefined|]) t₁
      t₃ = runHylo arithRed <$> t₂
      t₄ = (fmap $ fmap $ runHylo count) t₃
  in t₄ & runIdentity & observeInt

benchParam :: Int
benchParam = fromIntegral (maxBound :: Int16)

benchParam' :: [Int]
benchParam' = [ coeff i | i <- [1..benchParam]]
  where coeff :: Int -> Int
        coeff i | i `mod` 19 == 0 = 0
                | i `mod` 92 == 0 = 1
                | otherwise       = i

main :: IO ()
main = defaultMain
  [ bgroup "powerExpQ"
    [ bench "slow" (nfIO (measureExp (slowPowE [|10|] benchParam)))
    , bench "fast" (nfIO (measureExp (fastPowE [|10|] benchParam)))
    ]
  , bgroup "powerCount"
    [ bench "slow" (whnf slowPowC benchParam)
    , bench "fast" (whnf fastPowC benchParam) ]
  , bgroup "polyExpQ"
    [ bench "slow" (nfIO (measureExp (slowPolyE [|10|] benchParam')))
    , bench "fast" (nfIO (measureExp (fastPolyE [|10|] benchParam')))
    ]
  , bgroup "polyCount"
    [ bench "slow" (whnf slowPolyC benchParam')
    , bench "fast" (whnf fastPolyC benchParam') ]
  , bgroup "dotExpQ"
    [ bench "slow" (nfIO (measureExp (slowDotE [|10|] benchParam')))
    , bench "fast" (nfIO (measureExp (fastDotE [|10|] benchParam')))
    ]
  , bgroup "dotCount"
    [ bench "slow" (whnf slowDotC benchParam')
    , bench "fast" (whnf fastDotC benchParam') ]
  ]

measureSize :: Data a => a -> Int
measureSize ast = 1 + gmapQl (+) 0 measureSize ast

measureExp :: ExpQ -> IO Int
measureExp e = measureSize <$> runQ e
