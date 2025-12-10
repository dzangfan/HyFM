{-# LANGUAGE GADTs #-}

module Data.Functor.HyFM.Hylos where

newtype Fix f          = In { out :: f (Fix f) }

newtype Alg b f        = Alg (f b -> b)
newtype Coalg a f      = Coalg (a -> f a)
newtype BAlg g f₂ f₁   = BAlg (forall x. (f₂ x -> x) -> f₁ (g x) -> g x)
newtype DCoalg g f₁ f₂ = DCoalg (forall x. (x -> f₁ x) -> g x -> f₂ (g x))
newtype NT f₂ f₁       = NT (forall x. f₁ x -> f₂ x)

data Hylo alg f₂ f₁ coalg where
  Hφψ :: Alg b f₂ -> NT f₂ f₁ -> Coalg a f₁
    -> Hylo (Alg b) f₂ f₁ (Coalg a)
  Hφσ :: Alg b f₃ -> NT f₃ f₂ -> DCoalg g f₁ f₂
    -> Hylo (Alg b) f₃ f₂ (DCoalg g f₁)
  Hφo :: Alg b f₂ -> NT f₂ f₁
    -> Hylo (Alg b) f₂ f₁ Fix
  Hτψ :: BAlg g f₃ f₂ -> NT f₂ f₁ -> Coalg a f₁
    -> Hylo (BAlg g f₃) f₂ f₁ (Coalg a)
  Hτσ :: BAlg g₁ f₄ f₃ -> NT f₃ f₂ -> DCoalg g₂ f₁ f₂
    -> Hylo (BAlg g₁ f₄) f₃ f₂ (DCoalg g₂ f₁)
  Hτo :: BAlg g f₃ f₂ -> NT f₂ f₁
    -> Hylo (BAlg g f₃) f₂ f₁ Fix
  Hiψ :: NT f₂ f₁ -> Coalg a f₁
    -> Hylo Fix f₂ f₁ (Coalg a)
  Hiσ :: NT f₃ f₂ -> DCoalg g f₁ f₂
    -> Hylo Fix f₃ f₂ (DCoalg g f₁)
  Hio :: NT f₂ f₁ -> Hylo Fix f₂ f₁ Fix

newtype AlgM m b f = AlgM (f b -> m b)
newtype CoalgM m a f = CoalgM (a -> m (f a))
newtype BAlgM m g f₂ f₁
  = BAlgM (forall x. (f₂ x -> x) -> f₁ (g x) -> m (g x))
newtype DCoalgM m g f₁ f₂
  = DCoalgM (forall x. (x -> f₁ x) -> g x -> m (f₂ (g x)))

data HyloM algM f coalgM where
  HMφψ :: AlgM m b f -> CoalgM m a f
         -> HyloM (AlgM m b) f (CoalgM m a)
  HMφσ :: AlgM m b f₂ -> DCoalgM m g f₁ f₂
         -> HyloM (AlgM m b) f₂ (DCoalgM m g f₁)
  HMτψ :: BAlgM m g f₂ f₁ -> CoalgM m a f₁
         -> HyloM (BAlgM m g f₂) f₁ (CoalgM m a)
  HMτσ :: BAlgM m g₂ f₃ f₂ -> DCoalgM m g₁ f₁ f₂
         -> HyloM (BAlgM m g₂ f₃) f₂ (DCoalgM m g₁ f₁)
