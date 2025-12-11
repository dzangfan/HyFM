{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Data.Vector hiding (minimum)
import FloydWarshall
import StagedInstances
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary

instance Arbitrary a => Arbitrary (InfNum a) where
  arbitrary = Num <$> arbitrary

main :: IO ()
main = hspec $ do
  context "Matrix 1 (4 × 4)" $ do
    describe "Floyd-Warshall" $ do
      prop "computes the shortest path" $
        \v₁ v₂ v₃ v₄ ->
          fw1 (fromList [v₁, v₂, v₃, v₄]) `shouldBe`
          min (v₁ + v₃) (v₂ + v₄)
    describe "Floyd-Warshall (P)" $ do
      prop "computes the shortest path" $
        \v₁ v₂ v₃ v₄ ->
          fwP1 (fromList [v₁, v₂, v₃, v₄]) `shouldBe`
          min (v₁ + v₃) (v₂ + v₄)
    describe "Floyd-Warshall (P, M)" $ do
      prop "computes the shortest path" $
        \v₁ v₂ v₃ v₄ ->
          fwPM1 (fromList [v₁, v₂, v₃, v₄]) `shouldBe`
          min (v₁ + v₃) (v₂ + v₄)

  context "Matrix 2 (4 × 4)" $ do
    describe "Floyd-Warshall" $ do
      prop "computes the shortest path" $
        \v₁ v₂ v₃ v₄ v₅ ->
          fw2 (fromList [v₁, v₂, v₃, v₄, v₅]) `shouldBe`
          min (v₅ + v₃) (v₅ + v₃ + v₂ + v₅ + v₃)
    describe "Floyd-Warshall (P)" $ do
      prop "computes the shortest path" $
        \v₁ v₂ v₃ v₄ v₅ ->
          fwP2 (fromList [v₁, v₂, v₃, v₄, v₅]) `shouldBe`
          min (v₅ + v₃) (v₅ + v₃ + v₂ + v₅ + v₃)
    describe "Floyd-Warshall (P, M)" $ do
      prop "computes the shortest path" $
        \v₁ v₂ v₃ v₄ v₅ ->
          fwPM2 (fromList [v₁, v₂, v₃, v₄, v₅]) `shouldBe`
          min (v₅ + v₃) (v₅ + v₃ + v₂ + v₅ + v₃)

  context "Matrix 3 (7 × 7)" $ do
    describe "Floyd-Warshall" $ do
      prop "computes the shortest path" $
        \v₁ v₂ v₃ v₄ v₅ v₆ v₇ v₈ ->
          fw3 (fromList [v₁, v₂, v₃, v₄, v₅, v₆, v₇, v₈]) `shouldBe`
          minimum [ v₁ + v₃ + v₅ + v₇
                  , v₁ + v₃ + v₆ + v₈
                  , v₂ + v₄ + v₅ + v₇
                  , v₂ + v₄ + v₆ + v₈
                  ]
    describe "Floyd-Warshall (P)" $ do
      prop "computes the shortest path" $
        \v₁ v₂ v₃ v₄ v₅ v₆ v₇ v₈ ->
          fwP3 (fromList [v₁, v₂, v₃, v₄, v₅, v₆, v₇, v₈]) `shouldBe`
          minimum [ v₁ + v₃ + v₅ + v₇
                  , v₁ + v₃ + v₆ + v₈
                  , v₂ + v₄ + v₅ + v₇
                  , v₂ + v₄ + v₆ + v₈
                  ]
    describe "Floyd-Warshall (P, M)" $ do
      prop "computes the shortest path" $
        \v₁ v₂ v₃ v₄ v₅ v₆ v₇ v₈ ->
          fwPM3 (fromList [v₁, v₂, v₃, v₄, v₅, v₆, v₇, v₈]) `shouldBe`
          minimum [ v₁ + v₃ + v₅ + v₇
                  , v₁ + v₃ + v₆ + v₈
                  , v₂ + v₄ + v₅ + v₇
                  , v₂ + v₄ + v₆ + v₈
                  ]
