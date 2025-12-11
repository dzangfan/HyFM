
module Main where

import Criterion.Main
import Data.Vector (fromList)
import FloydWarshall
import StagedInstances

listOf :: Int -> [Int]
listOf n = take n [x `mod` 93 | x <- [1000,193..]]

main :: IO ()
main = defaultMain
  [ bgroup "mat1"
    [ bench "FW"        (whnf fw1   (fromList (Num <$> listOf 4)))
    , bench "FW (P)"    (whnf fwP1  (fromList (listOf 4)))
    , bench "FW (P, M)" (whnf fwPM1 (fromList (listOf 4)))]
  , bgroup "mat2"
    [ bench "FW"        (whnf fw2   (fromList (Num <$> listOf 5)))
    , bench "FW (P)"    (whnf fwP2  (fromList (listOf 5)))
    , bench "FW (P, M)" (whnf fwPM2 (fromList (listOf 5)))]
  , bgroup "mat3"
    [ bench "FW"        (whnf fw3   (fromList (Num <$> listOf 8)))
    , bench "FW (P)"    (whnf fwP3  (fromList (listOf 8)))
    , bench "FW (P, M)" (whnf fwPM3 (fromList (listOf 8)))]
  , bgroup "mat4"
    [ bench "FW (P)"    (whnf fwP4  (fromList (listOf 20)))
    , bench "FW (P, M)" (whnf fwPM4 (fromList (listOf 20)))]
  ]
