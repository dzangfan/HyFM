{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module StagedInstances where

import Collections
import Data.Vector (Vector)
import FloydWarshall

type FW  = Vector Int -> Int
type FW' = Vector (InfNum Int) -> InfNum Int


fw1 :: FW'
fw1 = $(compile₀ mat1 st1)

fwP1 :: FW
fwP1 = $(compile mat1 st1 fwComp₁)

fwPM1 :: FW
fwPM1 = $(compile mat1 st1 fwComp₃)


fw2 :: FW'
fw2 = $(compile₀ mat2 st2)

fwP2 :: FW
fwP2 = $(compile mat2 st2 fwComp₁)

fwPM2 :: FW
fwPM2 = $(compile mat2 st2 fwComp₃)


fw3 :: FW'
fw3 = $(compile₀ mat3 st3)

fwP3 :: FW
fwP3 = $(compile mat3 st3 fwComp₁)

fwPM3 :: FW
fwPM3 = $(compile mat3 st3 fwComp₃)


fwP4 :: FW
fwP4 = $(compile mat4 st4 fwComp₁)

fwPM4 :: FW
fwPM4 = $(compile mat4 st4 fwComp₃)
