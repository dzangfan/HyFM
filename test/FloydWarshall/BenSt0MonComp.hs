
module Main where

import Collections
import Criterion.Main
import Data.Data
import FloydWarshall
import Language.Haskell.TH

main :: IO ()
main = defaultMain
  [ bgroup "mat1"
    [ bench "F"  (nfIO (measureExp $ compile mat1 st1 fwComp₃))
    , bench "NF" (nfIO (measureExp $ compile mat1 st1 fwComp₄)) ]
  , bgroup "mat2"
    [ bench "F"  (nfIO (measureExp $ compile mat2 st2 fwComp₃))
    , bench "NF" (nfIO (measureExp $ compile mat2 st2 fwComp₄)) ]
  , bgroup "mat3"
    [ bench "F"  (nfIO (measureExp $ compile mat3 st3 fwComp₃))
    , bench "NF" (nfIO (measureExp $ compile mat3 st3 fwComp₄)) ]
  , bgroup "mat4"
    [ bench "F"  (nfIO (measureExp $ compile mat4 st4 fwComp₃))
    , bench "NF" (nfIO (measureExp $ compile mat4 st4 fwComp₄)) ]
  ]

measureSize :: Data a => a -> Int
measureSize ast = 1 + gmapQl (+) 0 measureSize ast

measureExp :: Q Exp -> IO Int
measureExp e = measureSize <$> runQ e
