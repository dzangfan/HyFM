
module Main where

import Collections
import Criterion.Main
import FloydWarshall

main :: IO ()
main = defaultMain
  [ bgroup "mat1"
    [ bench "F"  (whnf (stat mat1 st1) fwStat₁)
    , bench "NF" (whnf (stat mat1 st1) fwStat₂) ]
  , bgroup "mat2"
    [ bench "F"  (whnf (stat mat2 st2) fwStat₁)
    , bench "NF" (whnf (stat mat3 st3) fwStat₂) ]
  , bgroup "mat3"
    [ bench "F"  (whnf (stat mat3 st3) fwStat₁)
    , bench "NF" (whnf (stat mat3 st3) fwStat₂) ]
  , bgroup "mat4"
    [ bench "F"  (whnf (stat mat4 st4) fwStat₁)
    , bench "NF" (whnf (stat mat4 st4) fwStat₂) ]
  ]
