{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module FloydWarshall where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Functor
import Data.Functor.Compose
import Data.Functor.HyFM.Fusion
import Data.Functor.HyFM.Hylos
import Data.Functor.HyFM.Memo
import Data.Functor.HyFM.Zoo
import Data.Functor.Identity
import Data.Matrix hiding ((!))
import Data.Vector ((!))
import Language.Haskell.TH

data Weight v = Weight v | WeightInf

data Term v x
  = Var v | Lam (v -> x) | Let x (v -> x)
  | Inf   | Min x x      | Add x x
  deriving Functor
data Path v x = KIsZero (Weight v) | KIsPos x x x
  deriving (Functor, Foldable, Traversable)
instance Monad m => Distributive (Path v) m where
  dist = sequence

fwAna :: forall v. Matrix (Weight v)
  -> Hylo Fix (Path v) (Path v) (Coalg (Int, Int, Int))
fwAna mat = ana ψ
  where ψ (i, j, k)
          | k == 0 = KIsZero (getElem i j mat)
          | otherwise = KIsPos (i, j, k - 1)
                               (i, k, k - 1)
                               (k, j, k - 1)
        {-# INLINE ψ #-}
{-# INLINE fwAna #-}

fwCata :: forall v. Hylo (BAlg Identity (Term v)) (Path v) (Path v) Fix
fwCata = bcata τ
  where τ φ path = case path of
          KIsZero WeightInf  -> φ Inf
          KIsZero (Weight v) -> φ (Var v)
          KIsPos ij ik kj    -> φ (Min ij (φ (ik `Add` kj)))
        {-# INLINE τ #-}
{-# INLINE fwCata #-}

data PE v x = StInf | DyAtom v | Dy x
  deriving Functor

liftPE :: (Term v x -> x) -> PE v x -> x
liftPE φ = \case
  StInf    -> φ Inf
  DyAtom v -> φ (Var v)
  Dy     x -> x

termPE :: forall v.
  Hylo (BAlg (PE v) (Term v))
       (Term (PE v v)) (Term (PE v v))
       Fix
termPE = bcataF τ
  where
    τ φ term =
      case term of
        Var v -> φ . Var <$> v
        Lam h -> Dy (φ (Lam (\v -> liftPE φ $ h (DyAtom v))))
        Let StInf h -> h StInf
        Let (DyAtom v) h -> h (DyAtom v)
        Let (Dy x) h -> Dy (φ (Let x (\v -> liftPE φ (h (DyAtom v)))))
        Inf -> StInf
        Min StInf x -> x
        Min x StInf -> x
        Min x y -> Dy (φ (Min (liftPE φ x) (liftPE φ y)))
        Add StInf _ -> StInf
        Add _ StInf -> StInf
        Add x y -> Dy (φ (Add (liftPE φ x) (liftPE φ y)))
    {-# INLINE τ #-}
{-# INLINE termPE #-}

data InfNum a = Num a | Infty deriving (Functor)
instance Applicative InfNum where
  Num f <*> Num x = Num (f x)
  _ <*> _ = Infty
  pure = Num
instance Num a => Num (InfNum a) where
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = Num . fromInteger
  negate      = fmap negate
instance Eq a => Eq (InfNum a) where
  Num a == Num b = a == b
  _     == _     = False
instance Ord a => Ord (InfNum a) where
  Num a <= Num b = a <= b
  Num _ <= Infty = True
  Infty <= Num _ = False
  Infty <= Infty = False
instance Show a => Show (InfNum a) where
  show (Num a) = show a
  show Infty   = "∞"

termComp :: Hylo (Alg (Q Exp)) (Term (Q Exp)) (Term (Q Exp)) Fix
termComp = cata φ
  where φ term = case term of
          Var e   -> e
          Lam h   -> [| \x -> $(h [| x |]) |]
          Let x h -> [| let g = $x in $(h [|g|]) |]
          Min a b -> [| min $a $b |]
          Add a b -> [| $a + $b |]
          Inf     -> [| Infty |]
        {-# INLINE φ #-}
{-# INLINE termComp #-}

termStat :: Hylo (Alg (Int, Int)) (Term ()) (Term ()) Fix
termStat = cata φ
  where φ term = case term of
          Var () -> (0, 0)
          Lam h -> h ()
          Let x h -> x +♯ h ()
          Min a b -> a +♯ b +♯ (1, 0)
          Add a b -> a +♯ b +♯ (0, 1)
          Inf -> (0, 0)
        {-# INLINE φ #-}
        (a₁, b₁) +♯ (a₂, b₂) = (a₁ + a₂, b₁ + b₂)
{-# INLINE termStat #-}

-----------------------------------------------
--      Hylomorphism without Memoization     --
-----------------------------------------------

unwrapComp :: PE (Q Exp) (Q Exp) -> Q Exp
unwrapComp = \case
  Dy e     -> e
  DyAtom e -> e
  StInf    -> [| Infty |]

unwrapStat :: PE () (Int, Int) -> (Int, Int)
unwrapStat = \case
  Dy (m, a) -> (m, a)
  DyAtom () -> (0, 0)
  StInf     -> (0, 0)

-- compile, F ---------------------------------
fwComp₁ :: Matrix (Weight (PE (Q Exp) (Q Exp)))
        -> (Int, Int, Int)
        -> Q Exp
fwComp₁ mat = unwrapComp . runIdentity . getCompose . runHylo h
  where
    h :: Hylo (Alg (Compose Identity (PE (Q Exp)) (Q Exp)))
            (Path (PE (Q Exp) (Q Exp)))
            (Path (PE (Q Exp) (Q Exp)))
            (Coalg (Int, Int, Int))
    h = termComp ∘ termPE @(Q Exp) ∘ fwCata @(PE (Q Exp) (Q Exp)) ∘ fwAna mat

-- compile, NF ---------------------------------------------
fwComp₂ :: Matrix (Weight (PE (Q Exp) (Q Exp)))
        -> (Int, Int, Int)
        -> Q Exp
fwComp₂ mat = unwrapComp . runIdentity . fmap (fmap h₄) . fmap h₃ . h₂ . h₁
  where h₁ :: (Int, Int, Int) -> Fix (Path (PE (Q Exp) (Q Exp)))
        h₂ :: Fix (Path v) -> Identity (Fix (Term v))
        h₃ :: Fix (Term (PE v v)) -> PE v (Fix (Term v))
        h₄ :: Fix (Term (Q Exp)) -> Q Exp
        h₁ = runHylo (fwAna mat)
        h₂ = runHylo fwCata
        h₃ = runHylo termPE
        h₄ = runHylo termComp

-- count, F ------------------------------------------------------
fwStat₁ :: Matrix (Weight (PE () ()))
        -> (Int, Int, Int)
        -> (Int, Int)
fwStat₁ mat = unwrapStat . runIdentity . getCompose . runHylo h
  where
    h :: Hylo (Alg (Compose Identity (PE ()) (Int, Int)))
            (Path (PE () ()))
            (Path (PE () ()))
            (Coalg (Int, Int, Int))
    h = termStat ∘ termPE @() ∘ fwCata @(PE () ()) ∘ fwAna mat

-- count, NF ---------------------------------------------------------
fwStat₂ :: Matrix (Weight (PE () ()))
        -> (Int, Int, Int)
        -> (Int, Int)
fwStat₂ mat = unwrapStat . runIdentity . fmap (fmap h₄) . fmap h₃ . h₂ . h₁
  where h₁ :: (Int, Int, Int) -> Fix (Path (PE () ()))
        h₂ :: Fix (Path v) -> Identity (Fix (Term v))
        h₃ :: Fix (Term (PE v v)) -> PE v (Fix (Term v))
        h₄ :: Fix (Term ()) -> (Int, Int)
        h₁ = runHylo (fwAna mat)
        h₂ = runHylo fwCata
        h₃ = runHylo termPE
        h₄ = runHylo termStat

--------------------------------------------------------
--               Memoized Hylomorphisms              --
--------------------------------------------------------

-- compile, F ---------------------------------------------------
fwComp₃ :: Matrix (Weight (PE (Q Exp) (Q Exp)))
        -> (Int, Int, Int)
        -> Q Exp
fwComp₃ mat =   unwrapComp
              . runIdentity . getCompose
              . evalGenlet  . runHyloM h
  where h = memohylo $
              termComp
            ∘ termPE @(Q Exp)
            ∘ fwCata @(PE (Q Exp) (Q Exp))
            ∘ fwAna mat

instance Letable (PE (Q Exp) (Q Exp))
           (Compose Identity (PE (Q Exp)) (Q Exp))
           (Compose Identity (PE (Q Exp)) (Q Exp)) where
  mkVar v   = fwCompφ₃ (Var v)
  mkLet x h = fwCompφ₃ (Let x h)

fwCompH₃ :: Hylo (Alg (PE (Q Exp) (Q Exp)))
                 (Term (PE (Q Exp) (Q Exp)))
                 (Term (PE (Q Exp) (Q Exp)))
                 Fix
fwCompH₃ = termComp ∘ termPE @(Q Exp)

fwCompφ₃ :: Term (PE (Q Exp) (Q Exp)) (Compose Identity (PE (Q Exp)) (Q Exp))
  -> Compose Identity (PE (Q Exp)) (Q Exp)
fwCompφ₃ = liftAlg (Compose . Identity, runIdentity . getCompose) φ
  where (Alg φ, _) = unHylo fwCompH₃

-- compile, NF ------------------------------------------
fwComp₄ :: Matrix (Weight (PE (Q Exp) (Q Exp)))
        -> (Int, Int, Int)
        -> Q Exp
fwComp₄ mat = unwrapComp . evalGenlet . fmap (fmap h₃) . fmap h₂ . h₁
  where h₁ :: (Int, Int, Int)
           -> Genlet (Int, Int, Int) (PE (Q Exp) (Q Exp)) (PE (Q Exp) (Q Exp))
                     (Fix (Term (PE (Q Exp) (Q Exp))))
        h₂ :: Fix (Term (PE v v)) -> PE v (Fix (Term v))
        h₃ :: Fix (Term (Q Exp)) -> Q Exp
        h₁ = fmap runIdentity
             . runHyloM (memohylo (fwCata @(PE (Q Exp) (Q Exp)) ∘ fwAna mat))
        h₂ = runHylo termPE
        h₃ = runHylo termComp

instance Letable (PE (Q Exp) (Q Exp))
                 (Identity (Fix (Term (PE (Q Exp) (Q Exp)))))
                 (PE (Q Exp) (Q Exp)) where
  mkVar v = Identity (In (Var v))
  mkLet x h = fwCompφ₄ (Let (fwCompPE₄ (runIdentity x)) h)

fwCompPE₄ :: Fix (Term (PE (Q Exp) (Q Exp))) -> PE (Q Exp) (Q Exp)
fwCompPE₄ = runHylo fwCompH₃

fwCompφ₄ :: Term (PE (Q Exp) (Q Exp)) (PE (Q Exp) (Q Exp)) -> PE (Q Exp) (Q Exp)
(Alg fwCompφ₄, _) = unHylo fwCompH₃

-- compile, F ---------------------------------------------------
fwStat₃ :: Matrix (Weight (PE () ()))
        -> (Int, Int, Int)
        -> (Int, Int)
fwStat₃ mat =   unwrapStat
              . runIdentity . getCompose
              . evalGenlet  . runHyloM h
  where h = memohylo $
              termStat
            ∘ termPE @()
            ∘ fwCata @(PE () ())
            ∘ fwAna mat

instance Letable (PE () ())
           (Compose Identity (PE ()) (Int, Int))
           (Compose Identity (PE ()) (Int, Int)) where
  mkVar v   = fwStatφ₃ (Var v)
  mkLet x h = fwStatφ₃ (Let x h)

fwStatH₃ :: Hylo (Alg (PE () (Int, Int)))
                 (Term (PE () ()))
                 (Term (PE () ()))
                 Fix
fwStatH₃ = termStat ∘ termPE @()

fwStatφ₃ :: Term (PE () ()) (Compose Identity (PE ()) (Int, Int))
  -> Compose Identity (PE ()) (Int, Int)
fwStatφ₃ = liftAlg (Compose . Identity, runIdentity . getCompose) φ
  where (Alg φ, _) = unHylo fwStatH₃

-- count, NF --------------------------------------------
fwStat₄ :: Matrix (Weight (PE () ()))
        -> (Int, Int, Int)
        -> (Int, Int)
fwStat₄ mat = unwrapStat . evalGenlet . fmap (fmap h₃) . fmap h₂ . h₁
  where h₁ :: (Int, Int, Int)
           -> Genlet (Int, Int, Int) (PE () ()) (PE () (Int, Int))
                     (Fix (Term (PE () ())))
        h₂ :: Fix (Term (PE v v)) -> PE v (Fix (Term v))
        h₃ :: Fix (Term ()) -> (Int, Int)
        h₁ = fmap runIdentity
             . runHyloM (memohylo (fwCata @(PE () ()) ∘ fwAna mat))
        h₂ = runHylo termPE
        h₃ = runHylo termStat

instance Letable (PE () ())
                 (Identity (Fix (Term (PE () ()))))
                 (PE () (Int, Int)) where
  mkVar v = Identity (In (Var v))
  mkLet x h = fwStatφ₄ (Let (fwStatPE₄ (runIdentity x)) h)

fwStatPE₄ :: Fix (Term (PE () ())) -> PE () (Int, Int)
fwStatPE₄ = runHylo fwStatH₃

fwStatφ₄ :: Term (PE () ()) (PE () (Int, Int)) -> PE () (Int, Int)
(Alg fwStatφ₄, _) = unHylo fwStatH₃

-- Triggering compiler/counter -------------------------

compile
  :: Matrix Bool -> (Int, Int)
  -> (Matrix (Weight (PE (Q Exp) (Q Exp))) -> (Int, Int, Int) -> Q Exp)
  -> Q Exp
compile mat (i, j) c =
  [| \v -> $(let mat' = evalState (weighted [|v|]) 0
             in c mat' (i, j, sqrMatSz mat))|]
  where weighted :: Q Exp -> State Int (Matrix (Weight (PE (Q Exp) (Q Exp))))
        weighted v = forM mat $ \case
          False -> return WeightInf
          True  -> do
            idx <- get; modify succ
            return (Weight (DyAtom [| $v ! idx |]))

stat
  :: Matrix Bool -> (Int, Int)
  -> (Matrix (Weight (PE () ())) -> (Int, Int, Int) -> (Int, Int))
  -> (Int, Int)
stat mat (i, j) s = s weighted (i, j, sqrMatSz mat)
  where weighted = mat <&> \case
          True -> Weight (DyAtom ())
          False -> WeightInf

fwComp₀ :: Matrix (Weight (Q Exp))
        -> (Int, Int, Int)
        -> Q Exp
fwComp₀ mat = runIdentity . runHylo h
  where
    h :: Hylo (Alg (Identity (Q Exp)))
            (Path (Q Exp))
            (Path (Q Exp))
            (Coalg (Int, Int, Int))
    h = termComp ∘ fwCata @(Q Exp) ∘ fwAna mat

compile₀
  :: Matrix Bool -> (Int, Int) -> Q Exp
compile₀ mat (i, j) =
  [| \v -> $(let mat' = evalState (weighted [|v|]) 0
             in fwComp₀ mat' (i, j, sqrMatSz mat))|]
  where weighted :: Q Exp -> State Int (Matrix (Weight (Q Exp)))
        weighted v = forM mat $ \case
          False -> return WeightInf
          True  -> do
            idx <- get; modify succ
            return (Weight [| $v ! idx |])

sqrMatSz :: Matrix a -> Int
sqrMatSz mat
  | nrow == ncol = nrow
  | otherwise    = error "Not a square matrix"
  where nrow = nrows mat; ncol = ncols mat

