module Data.Vector 
( Vector(..) ) where

data Vector a = Vector a a a
    deriving (Show, Eq)

instance (Num a) => Num (Vector a) where
  (Vector x1 y1 z1) + (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)
  (Vector x1 y1 z1) * (Vector x2 y2 z2) = Vector (x1*x2) (y1*y2) (z1*z2)
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined

instance Functor Vector where
  fmap f (Vector x y z) = Vector (f x) (f y) (f z)
