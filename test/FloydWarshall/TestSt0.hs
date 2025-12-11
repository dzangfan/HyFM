
module Main where

import Collections
import FloydWarshall
import Language.Haskell.TH
import Test.Hspec

main :: IO ()
main = hspec $ do
  context "For matrix 1 (4 × 4)" $ do
    describe "stat (pure)" $ do
      it "can count (min, +) with fusion" $ do
        stat mat1 st1 fwStat₁ `shouldBe` (1, 2)
      it "can count (min, +) without fusion" $ do
        stat mat1 st1 fwStat₂ `shouldBe` (1, 2)
    describe "stat (monadic)" $ do
      it "can count (min, +) with fusion" $ do
        stat mat1 st1 fwStat₃ `shouldBe` (1, 2)
      it "can count (min, +) without fusion" $ do
        stat mat1 st1 fwStat₄ `shouldBe` (1, 2)
    describe "compile (pure)" $ do
      it "generates the same code regardless of fusion" $ do
        e₁ <- runQ $ compile mat1 st1 fwComp₁
        e₂ <- runQ $ compile mat1 st1 fwComp₂
        pprint e₁ `shouldBe` pprint e₂
    describe "compile (monadic)" $ do
      it "generates the same code regardless of fusion" $ do
        e₁ <- runQ $ compile mat1 st1 fwComp₃
        e₂ <- runQ $ compile mat1 st1 fwComp₄
        pprint e₁ `shouldBe` pprint e₂

  context "For matrix 2 (4 × 4)" $ do
    describe "stat (pure)" $ do
      it "can count (min, +) with fusion" $ do
        stat mat2 st2 fwStat₁ `shouldBe` (1, 5)
      it "can count (min, +) without fusion" $ do
        stat mat2 st2 fwStat₂ `shouldBe` (1, 5)
    describe "stat (monadic)" $ do
      it "can count (min, +) with fusion" $ do
        stat mat2 st2 fwStat₃ `shouldBe` (1, 4)
      it "can count (min, +) without fusion" $ do
        stat mat2 st2 fwStat₄ `shouldBe` (1, 4)
    describe "compile (pure)" $ do
      it "generates the same code regardless of fusion" $ do
        e₁ <- runQ $ compile mat2 st2 fwComp₁
        e₂ <- runQ $ compile mat2 st2 fwComp₂
        pprint e₁ `shouldBe` pprint e₂
    describe "compile (monadic)" $ do
      it "generates the same code regardless of fusion" $ do
        e₁ <- runQ $ compile mat2 st2 fwComp₃
        e₂ <- runQ $ compile mat2 st2 fwComp₄
        pprint e₁ `shouldBe` pprint e₂

  context "For matrix 3 (7 × 7)" $ do
    describe "stat (pure)" $ do
      it "can count (min, +) with fusion" $ do
        stat mat3 st3 fwStat₁ `shouldBe` (3, 8)
      it "can count (min, +) without fusion" $ do
        stat mat3 st3 fwStat₂ `shouldBe` (3, 8)
    describe "stat (monadic)" $ do
      it "can count (min, +) with fusion" $ do
        stat mat3 st3 fwStat₃ `shouldBe` (2, 6)
      it "can count (min, +) without fusion" $ do
        stat mat3 st3 fwStat₄ `shouldBe` (2, 6)
    describe "compile (pure)" $ do
      it "generates the same code regardless of fusion" $ do
        e₁ <- runQ $ compile mat3 st3 fwComp₁
        e₂ <- runQ $ compile mat3 st3 fwComp₂
        pprint e₁ `shouldBe` pprint e₂
    describe "compile (monadic)" $ do
      it "generates the same code regardless of fusion" $ do
        e₁ <- runQ $ compile mat3 st3 fwComp₃
        e₂ <- runQ $ compile mat3 st3 fwComp₄
        pprint e₁ `shouldBe` pprint e₂

  context "For matrix 4 (7 × 7)" $ do
    describe "stat (pure)" $ do
      it "can count (min, +) with fusion" $ do
        stat mat4 st4 fwStat₁ `shouldBe` (21, 32)
      it "can count (min, +) without fusion" $ do
        stat mat4 st4 fwStat₂ `shouldBe` (21, 32)
    describe "stat (monadic)" $ do
      it "can count (min, +) with fusion" $ do
        stat mat4 st4 fwStat₃ `shouldBe` (10, 15)
      it "can count (min, +) without fusion" $ do
        stat mat4 st4 fwStat₄ `shouldBe` (10, 15)
    describe "compile (pure)" $ do
      it "generates the same code regardless of fusion" $ do
        e₁ <- runQ $ compile mat4 st4 fwComp₁
        e₂ <- runQ $ compile mat4 st4 fwComp₂
        pprint e₁ `shouldBe` pprint e₂
    describe "compile (monadic)" $ do
      it "generates the same code regardless of fusion" $ do
        e₁ <- runQ $ compile mat4 st4 fwComp₃
        e₂ <- runQ $ compile mat4 st4 fwComp₄
        pprint e₁ `shouldBe` pprint e₂
