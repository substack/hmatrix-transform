{-# LANGUAGE FlexibleContexts #-}
module Numeric.LinearAlgebra.Transform (
    translate, rotate, scale
) where

import Numeric.LinearAlgebra

translate :: (Element e, Num (Matrix e), Linear Matrix e)
    => Vector e -> Matrix e -> Matrix e
translate v m = (translation v) * m

translation :: (Element e, Linear Matrix e) => Vector e -> Matrix e
translation v = fromLists $
    [ take (n + 1) $ drop i $ cycle $ 1 : replicate n 0 | i <- ii ]
    ++ [ toList v ++ [1] ]
    where
        n = dim v
        ii = [ n + 1, n .. 2 ]

rotate :: Linear c e => e -> Vector e -> c e -> c e
rotate = undefined

rotation :: Linear c e => e -> Vector e -> c e
rotation = undefined
