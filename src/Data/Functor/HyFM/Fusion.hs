{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Data.Functor.HyFM.Fusion where

import Data.Functor.HyFM.Hylos
import Data.Functor.Compose

infixr 9 ∘
class Fusion a b c | a b -> c where
  (∘) :: a -> b -> c

-- Cata-HyloFusionF
-- G〚φ, η₂, out〛 ∘ 〚τ in, η₁, ψ〛
instance Fusion
  (Hylo (Alg b) f₄ f₃ Fix) (Hylo (BAlg g f₃) f₂ f₁ coalg)
  (Hylo (Alg (g b)) f₂ f₁ coalg) where
  Hφo (Alg φ) (NT η) ∘ h = case h of
    Hτψ (BAlg τ) nt coalg -> Hφψ (Alg (τ (φ . η))) nt coalg
    Hτσ (BAlg τ) nt coalg -> Hφσ (Alg (τ (φ . η))) nt coalg
    Hτo (BAlg τ) nt       -> Hφo (Alg (τ (φ . η))) nt
  {-# INLINE (∘) #-}
-- G〚τ₂ in, η₂, out〛 ∘ 〚τ₁ in, η₁, ψ〛
instance Functor f₂ =>
  Fusion
  (Hylo (BAlg g₂ f₅) f₄ f₃ Fix) (Hylo (BAlg g₁ f₃) f₂ f₁ coalg)
  (Hylo (BAlg (Compose g₁ g₂) f₅) f₂ f₁ coalg) where
  Hτo (BAlg τ₂) (NT η) ∘ h = case h of
    Hτψ (BAlg τ₁) nt coalg -> Hτψ (BAlg (comτ₂ τ₁)) nt coalg
    Hτσ (BAlg τ₁) nt coalg -> Hτσ (BAlg (comτ₂ τ₁)) nt coalg
    Hτo (BAlg τ₁) nt       -> Hτo (BAlg (comτ₂ τ₁)) nt
    where
      comτ₂ :: (forall x. (f₃ x -> x) -> f₂ (g₁ x) -> g₁ x)
        -> (forall x. (f₅ x -> x) -> f₂ (Compose g₁ g₂ x) -> Compose g₁ g₂ x)
      τ'    :: forall x. (f₅ x -> x) -> f₃ (g₂ x) -> g₂ x
      comτ₂ τ₁ = liftAlg (Compose, getCompose) . τ₁ . τ'
      τ' x = τ₂ x . η
      {-# INLINE τ' #-}
      {-# INLINE comτ₂ #-}
  {-# INLINE (∘) #-}
-- G〚in, η₂, out〛 ∘ 〚τ₁ in, η₁, ψ〛
instance Fusion
  (Hylo Fix f₄ f₃ Fix) (Hylo (BAlg g f₃) f₂ f₁ coalg)
  (Hylo (BAlg g f₄) f₂ f₁ coalg) where
  Hio (NT η) ∘ h = case h of
    Hτψ (BAlg τ) nt coalg -> Hτψ (BAlg (τ . τ')) nt coalg
    Hτσ (BAlg τ) nt coalg -> Hτσ (BAlg (τ . τ')) nt coalg
    Hτo (BAlg τ) nt       -> Hτo (BAlg (τ . τ')) nt
    where τ' :: forall x. (f₄ x -> x) -> f₃ x -> x
          τ' x = x . η
          {-# INLINE τ' #-}
  {-# INLINE (∘) #-}
-- Neutral ones (Cata-HyloFusionF)
-- 〚φ, η₂, out〛 ∘ 〚in, η₁, ψ〛
instance Fusion
  (Hylo (Alg b) f₃ f₂ Fix) (Hylo Fix f₂ f₁ coalg)
  (Hylo (Alg b) f₂ f₁ coalg) where
  Hφo (Alg φ) (NT η) ∘ h = case h of
    Hiψ nt coalg -> Hφψ (Alg (φ . η)) nt coalg
    Hiσ nt coalg -> Hφσ (Alg (φ . η)) nt coalg
    Hio nt       -> Hφo (Alg (φ . η)) nt
  {-# INLINE (∘) #-}
-- 〚τ in, η₂, out〛 ∘ 〚in, η₁, ψ〛
instance Fusion
  (Hylo (BAlg g f₄) f₃ f₂ Fix) (Hylo Fix f₂ f₁ coalg)
  (Hylo (BAlg g f₄) f₂ f₁ coalg) where
  Hτo (BAlg τ) (NT η) ∘ h = case h of
    Hiψ nt coalg -> Hτψ (BAlg τ') nt coalg
    Hiσ nt coalg -> Hτσ (BAlg τ') nt coalg
    Hio nt       -> Hτo (BAlg τ') nt
    where τ' :: forall x. (f₄ x -> x) -> f₂ (g x) -> g x
          τ' x = τ x . η
          {-# INLINE τ' #-}
  {-# INLINE (∘) #-}
-- 〚in, η₂, out〛 ∘ 〚in, η₁, ψ〛
instance Fusion
  (Hylo Fix f₃ f₂ Fix) (Hylo Fix f₂ f₁ coalg)
  (Hylo Fix f₃ f₁ coalg) where
  Hio (NT η₂) ∘ h = case h of
    Hiψ (NT η₁) coalg -> Hiψ (NT (η₂ . η₁)) coalg
    Hiσ (NT η₁) coalg -> Hiσ (NT (η₂ . η₁)) coalg
    Hio (NT η₁)       -> Hio (NT (η₂ . η₁))
  {-# INLINE (∘) #-}

-- Hylo-AnaFusionF
-- 〚φ, η₂, σ out〛 ∘ G〚in, η₁, ψ〛
instance Fusion
  (Hylo alg f₄ f₃ (DCoalg g f₂)) (Hylo Fix f₂ f₁ (Coalg a))
  (Hylo alg f₄ f₃ (Coalg (g a))) where
  h ∘ Hiψ (NT η) (Coalg ψ) = case h of
    Hφσ alg nt (DCoalg σ) -> Hφψ alg nt (Coalg (σ (η . ψ)))
    Hτσ alg nt (DCoalg σ) -> Hτψ alg nt (Coalg (σ (η . ψ)))
    Hiσ nt (DCoalg σ)     -> Hiψ nt (Coalg (σ (η . ψ)))
  {-# INLINE (∘) #-}
-- 〚φ, η₂, σ₂ out〛 ∘ G〚in, η₁, σ₁ out〛
instance Functor f₄ =>
  Fusion
  (Hylo alg f₅ f₄ (DCoalg g₂ f₃)) (Hylo Fix f₃ f₂ (DCoalg g₁ f₁))
  (Hylo alg f₅ f₄ (DCoalg (Compose g₂ g₁) f₁)) where
  h ∘ Hiσ (NT η₁) (DCoalg σ₁) = case h of
    Hφσ alg nt (DCoalg σ₂) -> Hφσ alg nt (DCoalg (comσ₁ σ₂))
    Hτσ alg nt (DCoalg σ₂) -> Hτσ alg nt (DCoalg (comσ₁ σ₂))
    Hiσ nt (DCoalg σ₂)     -> Hiσ nt (DCoalg (comσ₁ σ₂))
    where comσ₁ :: (forall x. (x -> f₃ x) -> g₂ x -> f₄ (g₂ x))
            -> forall x. (x -> f₁ x) -> Compose g₂ g₁ x -> f₄ (Compose g₂ g₁ x)
          σ' :: forall x. (x -> f₁ x) -> g₁ x -> f₃ (g₁ x)
          comσ₁ σ₂ = liftCoalg (Compose, getCompose) . σ₂ . σ'
          σ' x = η₁ . σ₁ x
          {-# INLINE σ' #-}
          {-# INLINE comσ₁ #-}
  {-# INLINE (∘) #-}
-- 〚φ, η₂, σ₂ out〛 ∘ G〚in, η₁, out〛
instance Fusion
  (Hylo alg f₄ f₃ (DCoalg g f₂)) (Hylo Fix f₂ f₁ Fix)
  (Hylo alg f₄ f₃ (DCoalg g f₁)) where
  h ∘ Hio (NT η) = case h of
    Hφσ alg nt (DCoalg σ) -> Hφσ alg nt (DCoalg (σ . σ'))
    Hτσ alg nt (DCoalg σ) -> Hτσ alg nt (DCoalg (σ . σ'))
    Hiσ nt (DCoalg σ) -> Hiσ nt (DCoalg (σ . σ'))
    where σ' :: forall x. (x -> f₁ x) -> x -> f₂ x
          σ' x = η . x
          {-# INLINE σ' #-}
  {-# INLINE (∘) #-}

-- Cata-HyloFusionFM
-- M(G〚φ, η, out〛) ∘ 〚τ in, ψ〛ᴹ
instance Fusion
  (Hylo (Alg b) f₃ f₂ Fix) (HyloM (BAlgM m g f₂) f₁ coalg)
  (HyloM (AlgM m (g b)) f₁ coalg) where
  Hφo (Alg φ) (NT η) ∘ h = case h of
    HMτψ (BAlgM τ) coalg -> HMφψ (AlgM (τ (φ . η))) coalg
    HMτσ (BAlgM τ) coalg -> HMφσ (AlgM (τ (φ . η))) coalg
  {-# INLINE (∘) #-}
-- M(G〚τ₂ in, η, out〛) ∘ 〚τ₁ in, ψ〛ᴹ
instance (Functor f₁, Functor m) =>
  Fusion
  (Hylo (BAlg g₂ f₄) f₃ f₂ Fix) (HyloM (BAlgM m g₁ f₂) f₁ coalg)
  (HyloM (BAlgM m (Compose g₁ g₂) f₄) f₁ coalg) where
  Hτo (BAlg τ₂) (NT η) ∘ h = case h of
    HMτψ (BAlgM τ₁) coalg -> HMτψ (BAlgM (comτ₂ τ₁)) coalg
    HMτσ (BAlgM τ₁) coalg -> HMτσ (BAlgM (comτ₂ τ₁)) coalg
    where comτ₂ :: (forall x. (f₂ x -> x) -> f₁ (g₁ x) -> m (g₁ x)) ->
                    forall x. (f₄ x -> x)
                 -> f₁ (Compose g₁ g₂ x) -> m (Compose g₁ g₂ x)
          τ' :: forall x. (f₄ x -> x) -> f₂ (g₂ x) -> g₂ x
          comτ₂ τ₁ = liftAlgM (Compose, getCompose) . τ₁ . τ'
          τ' x = τ₂ x . η
          {-# INLINE τ' #-}
          {-# INLINE comτ₂ #-}
  {-# INLINE (∘) #-}
-- M(G〚in, η, out〛) ∘ 〚τ in, ψ〛ᴹ
instance Fusion
  (Hylo Fix f₃ f₂ Fix) (HyloM (BAlgM m g f₂) f₁ coalg)
  (HyloM (BAlgM m g f₃) f₁ coalg) where
  Hio (NT η) ∘ h = case h of
    HMτψ (BAlgM τ) coalg -> HMτψ (BAlgM (τ . τ')) coalg
    HMτσ (BAlgM τ) coalg -> HMτσ (BAlgM (τ . τ')) coalg
    where τ' :: forall x. (f₃ x -> x) -> f₂ x -> x
          τ' x = x . η
          {-# INLINE τ' #-}
  {-# INLINE (∘) #-}
-- Hylo-AnaFusionFM
-- 〚φ, σ out〛ᴹ ∘ G(〚in, η, ψ〛)
instance Fusion
  (HyloM algM f₃ (DCoalgM m g f₂)) (Hylo Fix f₂ f₁ (Coalg a))
  (HyloM algM f₃ (CoalgM m (g a))) where
  h ∘ Hiψ (NT η) (Coalg ψ) = case h of
    HMφσ alg (DCoalgM σ) -> HMφψ alg (CoalgM (σ (η . ψ)))
    HMτσ alg (DCoalgM σ) -> HMτψ alg (CoalgM (σ (η . ψ)))
  {-# INLINE (∘) #-}
-- 〚φ, σ₂ out〛ᴹ ∘ G(〚in, η, σ₁ out〛)
instance (Functor f₄, Functor m) =>
  Fusion
  (HyloM algM f₄ (DCoalgM m g₂ f₃)) (Hylo Fix f₃ f₂ (DCoalg g₁ f₁))
  (HyloM algM f₄ (DCoalgM m (Compose g₂ g₁) f₁)) where
  h ∘ Hiσ (NT η) (DCoalg σ₁) = case h of
    HMφσ alg (DCoalgM σ₂) -> HMφσ alg (DCoalgM (comσ₁ σ₂))
    HMτσ alg (DCoalgM σ₂) -> HMτσ alg (DCoalgM (comσ₁ σ₂))
    where comσ₁ :: (forall x. (x -> f₃ x) -> g₂ x -> m (f₄ (g₂ x)))
            -> forall x. (x -> f₁ x)
            -> Compose g₂ g₁ x -> m (f₄ (Compose g₂ g₁ x))
          σ' :: forall x. (x -> f₁ x) -> g₁ x -> f₃ (g₁ x)
          comσ₁ σ₂ = liftCoalgM (Compose, getCompose) . σ₂ . σ'
          σ' x = η . σ₁ x
          {-# INLINE comσ₁ #-}
          {-# INLINE σ' #-}
  {-# INLINE (∘) #-}
-- 〚φ, σ₂ out〛ᴹ ∘ G(〚in, η, out〛)
instance Fusion
  (HyloM algM f₃ (DCoalgM m g f₂)) (Hylo Fix f₂ f₁ Fix)
  (HyloM algM f₃ (DCoalgM m g f₁)) where
  h ∘ Hio (NT η) = case h of
    HMφσ alg (DCoalgM σ) -> HMφσ alg (DCoalgM (σ . σ'))
    HMτσ alg (DCoalgM σ) -> HMτσ alg (DCoalgM (σ . σ'))
    where σ' :: forall x. (x -> f₁ x) -> x -> f₂ x
          σ' x = η . x
          {-# INLINE σ' #-}
  {-# INLINE (∘) #-}

liftAlg :: Functor f
        => (a -> b, b -> a)
        -> (f a -> a)
        -> (f b -> b)
liftAlg (i, j) h = i . h . fmap j
liftCoalg :: Functor f
          => (a -> b, b -> a)
          -> (a -> f a)
          -> (b -> f b)
liftCoalg (i, j) h = fmap i . h . j

liftAlgM :: (Functor f, Functor m)
         => (a -> b, b -> a)
         -> (f a -> m a)
         -> (f b -> m b)
liftAlgM (i, j) h = fmap i . h . fmap j
liftCoalgM :: (Functor f, Functor m)
           => (a -> b, b -> a)
           -> (a -> m (f a))
           -> (b -> m (f b))
liftCoalgM (i, j) h = fmap (fmap i) . h . j
{-# INLINE liftAlg #-}
{-# INLINE liftCoalg #-}
{-# INLINE liftAlgM #-}
{-# INLINE liftCoalgM #-}
