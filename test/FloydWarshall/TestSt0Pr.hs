
module Main where

import Collections
import Data.Data
import FloydWarshall
import Language.Haskell.TH

main :: IO ()
main = do
  putStrLn "Matrix 1"
  putStrLn ("(F,  min, +) = " ++ show (stat mat1 st1 fwStat₁))
  putStrLn ("(NF, min, +) = " ++ show (stat mat1 st1 fwStat₂))
  szF₁  <- measureExp (compile mat1 st1 fwComp₁)
  szNF₁ <- measureExp (compile mat1 st1 fwComp₂)
  putStrLn ("(F,  code size) = " ++ show szF₁)
  putStrLn ("(NF, code size) = " ++ show szNF₁)
  e₁ <- runQ (compile mat1 st1 fwComp₁)
  putStrLn (pprint e₁ ++ "\n")
  
  putStrLn "Matrix 2"
  putStrLn ("(F,  min, +) = " ++ show (stat mat2 st2 fwStat₁))
  putStrLn ("(NF, min, +) = " ++ show (stat mat2 st2 fwStat₂))
  szF₂  <- measureExp (compile mat2 st2 fwComp₁)
  szNF₂ <- measureExp (compile mat2 st2 fwComp₂)
  putStrLn ("(F,  code size) = " ++ show szF₂)
  putStrLn ("(NF, code size) = " ++ show szNF₂)
  e₂ <- runQ (compile mat2 st2 fwComp₁)
  putStrLn (pprint e₂ ++ "\n")
  
  putStrLn "Matrix 3"
  putStrLn ("(F,  min, +) = " ++ show (stat mat3 st3 fwStat₁))
  putStrLn ("(NF, min, +) = " ++ show (stat mat3 st3 fwStat₂))
  szF₃  <- measureExp (compile mat3 st3 fwComp₁)
  szNF₃ <- measureExp (compile mat3 st3 fwComp₂)
  putStrLn ("(F,  code size) = " ++ show szF₃)
  putStrLn ("(NF, code size) = " ++ show szNF₃)
  e₃ <- runQ (compile mat3 st3 fwComp₁)
  putStrLn (pprint e₃ ++ "\n")

  putStrLn "Matrix 4"
  putStrLn ("(F,  min, +) = " ++ show (stat mat4 st4 fwStat₁))
  putStrLn ("(NF, min, +) = " ++ show (stat mat4 st4 fwStat₂))
  szF₄  <- measureExp (compile mat4 st4 fwComp₁)
  szNF₄ <- measureExp (compile mat4 st4 fwComp₂)
  putStrLn ("(F,  code size) = " ++ show szF₄)
  putStrLn ("(NF, code size) = " ++ show szNF₄)
  e₄ <- runQ (compile mat4 st4 fwComp₁)
  putStrLn (pprint e₄)

measureSize :: Data a => a -> Int
measureSize ast = 1 + gmapQl (+) 0 measureSize ast

measureExp :: Q Exp -> IO Int
measureExp e = measureSize <$> runQ e
