module Nat
    ( toInteger'
    , Nat(..)
    ) where

data Nat = Z | S Nat
    deriving (Show)

toInteger' :: Num t => Nat -> t
toInteger' Z     = 0
toInteger' (S x) = 1 + toInteger' x

instance Eq Nat where
    (==) Z Z         = True
    (==) Z _         = False
    (==) _ Z         = False
    (==) (S x) (S y) = x == y

instance Ord Nat where
    (<=) Z Z         = True
    (<=) Z _         = True
    (<=) _ Z         = False
    (<=) (S x) (S y) = x <= y

instance Num Nat where
    (+) x Z     = x
    (+) x (S y) = S (x + y)
    (-) x Z         = x
    (-) Z _         = error "b > a"
    (-) (S x) (S y) = x - y
    (*) x Z     = Z
    (*) x (S y) = x + (x * y)
    abs x = x
    signum Z = Z
    signum _ = S Z
    fromInteger 0 = Z
    fromInteger n = S (fromInteger (n - 1))
