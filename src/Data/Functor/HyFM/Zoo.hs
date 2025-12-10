{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.Functor.HyFM.Zoo where

import Control.Monad
import Data.Function
import Data.Functor.HyFM.Fusion
import Data.Functor.HyFM.Hylos
import Data.Functor.Identity

class IsHylo h f a b | h -> f a b where
  unHylo :: h -> (Alg b f, Coalg a f)
runHylo :: Functor f => IsHylo h f a b => h -> a -> b
runHylo h = fix (\f -> φ . fmap f . ψ)
  where (Alg φ, Coalg ψ) = unHylo h
{-# INLINE runHylo #-}

class Distributive f m where
  dist :: f (m a) -> m (f a)
class IsHyloM h f m a b | h -> f m a b where
  unHyloM :: h -> (AlgM m b f, CoalgM m a f)
runHyloM :: (Monad m, Functor f, Distributive f m, IsHyloM h f m a b)
      => h -> a -> m b
runHyloM h = fix (\f -> φ <=< (dist . fmap f) <=< ψ)
  where (AlgM φ, CoalgM ψ) = unHyloM h
{-# INLINE runHyloM #-}

ana :: (a -> f a) -> Hylo Fix f f (Coalg a)
ana ψ = Hiψ (NT id) (Coalg ψ)
{-# INLINE ana #-}
cata :: (f b -> b) -> Hylo (Alg b) f f Fix
cata φ = Hφo (Alg φ) (NT id)
{-# INLINE cata #-}

dana :: Functor f₂
     => (forall x. (x -> f₁ x) -> x -> f₂ x)
     -> Hylo Fix f₂ f₂ (DCoalg Identity f₁)
dana σ = danaF (liftCoalg (Identity, runIdentity) . σ)
{-# INLINE dana #-}
bcata :: Functor f₁
      => (forall x. (f₂ x -> x) -> f₁ x -> x)
      -> Hylo (BAlg Identity f₂) f₁ f₁ Fix
bcata τ = bcataF (liftAlg (Identity, runIdentity) . τ)
{-# INLINE bcata #-}

danaF :: (forall x. (x -> f₁ x) -> g x -> f₂ (g x))
      -> Hylo Fix f₂ f₂ (DCoalg g f₁)
danaF σ = Hiσ (NT id) (DCoalg σ)
{-# INLINE danaF #-}
bcataF :: (forall x. (f₂ x -> x) -> f₁ (g x) -> g x)
      -> Hylo (BAlg g f₂) f₁ f₁ Fix
bcataF τ = Hτo (BAlg τ) (NT id)
{-# INLINE bcataF #-}

hyloM :: (f b -> m b, a -> m (f a))
       -> HyloM (AlgM m b) f (CoalgM m a)
hyloM (φ, ψ) = HMφψ (AlgM φ) (CoalgM ψ)
{-# INLINE hyloM #-}
anaM :: (Functor f, Monad m)
     => (a -> m (f a))
     -> HyloM (BAlgM m Identity f) f (CoalgM m a)
anaM ψ = hyloMτ ((return.), ψ)
{-# INLINE anaM #-}
cataM :: (Functor f, Monad m)
      => (f b -> m b)
      -> HyloM (AlgM m b) f (DCoalgM m Identity f)
cataM φ = hyloMσ (φ, (return.))
{-# INLINE cataM #-}

hyloFMτ :: (forall x. (f₂ x -> x) -> f₁ (g x) -> m (g x), a -> m (f₁ a))
  -> HyloM (BAlgM m g f₂) f₁ (CoalgM m a)
hyloFMτ (τ, ψ) = HMτψ (BAlgM τ) (CoalgM ψ)
{-# INLINE hyloFMτ #-}
hyloMτ :: (Functor f₁, Monad m)
  => (forall x. (f₂ x -> x) -> f₁ x -> m x, a -> m (f₁ a))
  -> HyloM (BAlgM m Identity f₂) f₁ (CoalgM m a)
hyloMτ (τ, ψ) = hyloFMτ (liftAlgM (Identity, runIdentity) . τ, ψ)
{-# INLINE hyloMτ #-}

hyloFMσ :: (f₂ b -> m b, forall x. (x -> f₁ x) -> g x -> m (f₂ (g x)))
  -> HyloM (AlgM m b) f₂ (DCoalgM m g f₁)
hyloFMσ (φ, σ) = HMφσ (AlgM φ) (DCoalgM σ)
{-# INLINE hyloFMσ #-}
hyloMσ :: (Functor f₂, Monad m)
  => (f₂ b -> m b, forall x. (x -> f₁ x) -> x -> m (f₂ x))
  -> HyloM (AlgM m b) f₂ (DCoalgM m Identity f₁)
hyloMσ (φ, σ) = hyloFMσ (φ, liftCoalgM (Identity, runIdentity) . σ)
{-# INLINE hyloMσ #-}

hyloFMτσ :: ( forall x. (f₃ x -> x) -> f₂ (g₂ x) -> m (g₂ x),
                forall x. (x -> f₁ x) -> g₁ x -> m (f₂ (g₁ x)))
  -> HyloM (BAlgM m g₂ f₃) f₂ (DCoalgM m g₁ f₁)
hyloFMτσ (τ, σ) = HMτσ (BAlgM τ) (DCoalgM σ)
{-# INLINE hyloFMτσ #-}
hyloMτσ :: (Functor f₂, Monad m)
        => ( forall x. (f₃ x -> x) -> f₂ x -> m x
           , forall x. (x -> f₁ x) -> x -> m (f₂ x))
  -> HyloM (BAlgM m Identity f₃) f₂ (DCoalgM m Identity f₁)
hyloMτσ (τ, σ) = hyloFMτσ ( liftAlgM   (Identity, runIdentity) . τ
                          , liftCoalgM (Identity, runIdentity) . σ)
{-# INLINE hyloMτσ #-}

instance IsHylo (Hylo (Alg b) f₂ f₁ (Coalg a)) f₁ a b where
  unHylo (Hφψ (Alg φ) (NT η) (Coalg ψ)) = (Alg (φ . η), Coalg ψ)
  {-# INLINE unHylo #-}
instance IsHylo (Hylo (Alg b) f₂ f₁ (DCoalg g f₀)) f₁ (g (Fix f₀)) b where
  unHylo h = unHylo h'
    where h' :: Hylo (Alg b) f₂ f₁ (Coalg (g (Fix f₀)))
          h' = h ∘ ana (out @f₀)
          {-# INLINE h' #-}
  {-# INLINE unHylo #-}
instance IsHylo (Hylo (Alg b) f₂ f₁ Fix) f₁ (Fix f₁) b where
  unHylo h = unHylo h'
    where h' :: Hylo (Alg b) f₁ f₁ (Coalg (Fix f₁))
          h' = h ∘ ana (out @f₁)
          {-# INLINE h' #-}
  {-# INLINE unHylo #-}
instance IsHylo (Hylo (BAlg g f₃) f₂ f₁ (Coalg a)) f₁ a (g (Fix f₃)) where
  unHylo h = unHylo h'
    where h' :: Hylo (Alg (g (Fix f₃))) f₂ f₁ (Coalg a)
          h' = cata (In @f₃) ∘ h
          {-# INLINE h' #-}
  {-# INLINE unHylo #-}
instance IsHylo (Hylo (BAlg g₂ f₄) f₃ f₂ (DCoalg g₁ f₁))
         f₂ (g₁ (Fix f₁)) (g₂ (Fix f₄)) where
  unHylo h = unHylo h'
    where h' :: Hylo (Alg (g₂ (Fix f₄))) f₃ f₂ (Coalg (g₁ (Fix f₁)))
          h' = cata (In @f₄) ∘ h ∘ ana (out @f₁)
          {-# INLINE h' #-}
  {-# INLINE unHylo #-}
instance IsHylo (Hylo (BAlg g f₃) f₂ f₁ Fix) f₁ (Fix f₁) (g (Fix f₃)) where
  unHylo h = unHylo h'
    where h' :: Hylo (Alg (g (Fix f₃))) f₂ f₁ Fix
          h' = cata (In @f₃) ∘ h
          {-# INLINE h' #-}
  {-# INLINE unHylo #-}
instance IsHylo (Hylo Fix f₂ f₁ (Coalg a)) f₁ a (Fix f₂) where
  unHylo h = unHylo h'
    where h' :: Hylo (Alg (Fix f₂)) f₂ f₁ (Coalg a)
          h' = cata (In @f₂) ∘ h
          {-# INLINE h' #-}
  {-# INLINE unHylo #-}
instance IsHylo (Hylo Fix f₃ f₂ (DCoalg g f₁)) f₂ (g (Fix f₁)) (Fix f₃) where
  unHylo h = unHylo h'
    where h' :: Hylo (Alg (Fix f₃)) f₃ f₂ (DCoalg g f₁)
          h' = cata (In @f₃) ∘ h
          {-# INLINE h' #-}
  {-# INLINE unHylo #-}
instance IsHylo (Hylo Fix f₂ f₁ Fix) f₁ (Fix f₁) (Fix f₂) where
  unHylo h = unHylo h'
    where h' :: Hylo (Alg (Fix f₂)) f₂ f₁ (Coalg (Fix f₁))
          h' = cata (In @f₂) ∘ h ∘ ana (out @f₁)
          {-# INLINE h' #-}
  {-# INLINE unHylo #-}

instance IsHyloM (HyloM (AlgM m b) f (CoalgM m a)) f m a b where
  unHyloM (HMφψ alg coalg) = (alg, coalg)
  {-# INLINE unHyloM #-}
instance IsHyloM
  (HyloM (AlgM m b) f₂ (DCoalgM m g f₁)) f₂ m (g (Fix f₁)) b where
  unHyloM h = unHyloM h'
    where h' :: HyloM (AlgM m b) f₂ (CoalgM m (g (Fix f₁)))
          h' = h ∘ ana (out @f₁)
          {-# INLINE h' #-}
  {-# INLINE unHyloM #-}
instance IsHyloM
  (HyloM (BAlgM m g f₂) f₁ (CoalgM m a)) f₁ m a (g (Fix f₂)) where
  unHyloM h = unHyloM h'
    where h' :: HyloM (AlgM m (g (Fix f₂))) f₁ (CoalgM m a)
          h' = cata (In @f₂) ∘ h
          {-# INLINE h' #-}
  {-# INLINE unHyloM #-}
instance IsHyloM
  (HyloM (BAlgM m g₂ f₃) f₂ (DCoalgM m g₁ f₁))
  f₂ m (g₁ (Fix f₁)) (g₂ (Fix f₃)) where
  unHyloM h = unHyloM h'
    where h' :: HyloM (AlgM m (g₂ (Fix f₃))) f₂ (CoalgM m (g₁ (Fix f₁)))
          h' = cata (In @f₃) ∘ h ∘ ana (out @f₁)
          {-# INLINE h' #-}
  {-# INLINE unHyloM #-}
