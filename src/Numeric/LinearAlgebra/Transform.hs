{-# LANGUAGE FlexibleContexts #-}
module Numeric.LinearAlgebra.Transform (
    Rotation(..),
    translate, translation, rotate, rotation
) where

import Numeric.LinearAlgebra
import Control.Arrow ((&&&))

-- |Multiply a matrix with a translation, in that order.
translate :: (Field e, Num (Matrix e), Linear Matrix e)
    => Vector e -> Matrix e -> Matrix e
translate v m = m <> (translation v)

-- |Build a translation matrix in homogeneous coordinates.
translation :: Element e => Vector e -> Matrix e
translation v = takeColumns n (ident 4) <|> (asColumn v <-> (1 |> [1]))
    where n = dim v

-- |Specify the type of rotation for the rotation functions.
-- All angles are in radians.
data (Num e, Num (Vector e)) => Rotation e
    = AxisAngle e (Vector e) -- ^ Rotation about the supplied axis
    | AxisX e -- ^ Rotation about the x-axis
    | AxisY e -- ^ Rotation about the y-axis
    | AxisZ e -- ^ Rotation about the Z-axis
    | YawPitchRoll e e e -- ^ Rotation from right-handed, zxz Euler angle

-- |Multiply a matrix with a rotation, in that order.
rotate :: (Num (Matrix e), Field e, Num (Vector e), Linear Matrix e) =>
    Rotation e -> Matrix e -> Matrix e
rotate r m = m <> (rotation r)

-- |Build a rotation matrix in homogeneous coordinates.
rotation :: (Num (Vector e), Field e, Linear Matrix e) => Rotation e -> Matrix e
rotation (AxisAngle a axis) = case toList axis of
    [x,y,z] -> -- lifted from Data.Vec.LinAlg.Transform3D by Tobias Bexelius
        (4 >< 4) [
            x^2+(1-x^2)*c, x*y*(1-c)-z*s, x*z*(1-c)+y*s, 0,
            x*y*(1-c)+z*s, y^2+(1-y^2)*c, y*z*(1-c)-x*s, 0,
            x*z*(1-c)-y*s, y*z*(1-c)+x*s, z^2+(1-z^2)*c, 0,
            0, 0, 0, 1
        ]
    [x,y] ->
        (3 >< 3) [
            c, -s, 0,
            s, c, 0,
            0, 0, 1
        ]
    xs -> error $ (show $ length xs)
        ++ "-dimensional rotation is not implemented"
    where (c,s) = cos &&& sin $ a
rotation (AxisX a) =
    (4 >< 4) [
        1, 0, 0, 0,
        0, c, -s, 0,
        0, s, c, 0,
        0, 0, 0, 1
    ] where (c,s) = cos &&& sin $ a
rotation (AxisY a) =
    (4 >< 4) [
        c, 0, s, 0,
        0, 1, 0, 0,
        -s, 0, c, 0,
        0, 0, 0, 1
    ] where (c,s) = cos &&& sin $ a
rotation (AxisZ a) =
    (4 >< 4) [
        c, -s, 0, 0,
        s, c, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    ] where (c,s) = cos &&& sin $ a
rotation (YawPitchRoll x y z) =
    (rotation $ AxisZ z) <> (rotation $ AxisY y) <> (rotation $ AxisX x)
