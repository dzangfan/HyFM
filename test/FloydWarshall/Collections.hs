{-# LANGUAGE LambdaCase #-}

module Collections where

import Data.Matrix

mat1 :: Matrix Bool
mat1 = fromList 4 4
  [ False, True, True, False
  , False, False, False, True
  , False, False, False, True
  , False, False, False, False ]
st1 :: (Int, Int)
st1 = (1, 4)

mat2 :: Matrix Bool
mat2 = matrix 4 4 $ \case
  (2, 3) -> True; (2, 4) -> True
  (3, 2) -> True
  (4, 1) -> True; (4, 3) -> True
  _ -> False
st2 :: (Int, Int)
st2 = (4, 2)

mat3 :: Matrix Bool
mat3 = matrix 7 7 $ \case
  (1, 2) -> True; (1, 3) -> True; (2, 4) -> True; (3, 4) -> True
  (4, 5) -> True; (4, 6) -> True; (5, 7) -> True; (6, 7) -> True
  _ -> False
st3 :: (Int, Int)
st3 = (1, 7)

mat4 :: Matrix Bool
mat4 = matrix 16 16 $ \case
  (1, 5) -> True; (5, 9) -> True; (9, 10) -> True
  (10, 14) -> True; (14, 15) -> True; (15, 11) -> True; (11, 10) -> True
  (11, 12) -> True; (12, 8) -> True; (8, 4) -> True
  _ -> False
st4 :: (Int, Int)
st4 = (1, 4)
