{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.Functor.HyFM.Memo where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.State
import Data.Functor.HyFM.Hylos
import Data.Functor.HyFM.Zoo
import Data.Map
import Prelude hiding (lookup)

type MemoT k v m = StateT (Map k v) m
evalMemoT :: Monad m => MemoT k v m a -> m a
evalMemoT = flip evalStateT empty
memo :: (Monad m, Ord k) => k -> m v -> MemoT k v m v
memo k mv = do
  cache <- get
  case lookup k cache of
    Nothing -> do
      v₁ <- lift mv
      put (insert k v₁ cache)
      return v₁
    Just v₁ -> return v₁

type Genlet k v r = MemoT k v (Cont r)
evalGenlet :: Genlet k v b b -> b
evalGenlet = evalCont . evalMemoT

class Letable v a r | a -> v r where
  mkVar :: v -> a
  mkLet :: a -> (v -> r) -> r
genlet :: Letable v a r => a -> Cont r v
genlet a = shift $ \k -> return (mkLet a k)

newtype SndF f a b = SndF (a, f b)
  deriving Functor
instance (Monad m, Distributive f m) =>
  Distributive (SndF f a) m where
  dist (SndF (a, fb)) = SndF . (a,) <$> dist fb
  {-# INLINE dist #-}

memohylo :: (Ord a, Letable v b r, IsHylo h f a b)
         => h
         -> HyloM (AlgM (Genlet a v r) b)
                  (SndF f a)
                  (CoalgM (Genlet a v r) a)
memohylo h = HMφψ (AlgM φ') (CoalgM ψ')
  where (Alg φ, Coalg ψ) = unHylo h
        ψ' a = return (SndF (a, ψ a))
        φ' (SndF (a, fb)) =
          mkVar <$> memo a (genlet (φ fb))
{-# INLINE memohylo #-}
