module Vector
    ( vector3D
    , vector2D
    , vectLen
    , vectSum
    , vectScal
    , vectVect
    ) where

data Vector a = Vector
    { x :: a
    , y :: a
    , z :: a
    } deriving (Show)

vector3D :: a -> a -> a -> Vector a
vector3D x y z = Vector x y z

vector2D :: Num a => a -> a -> Vector a
vector2D x y = Vector x y 0

vectLen :: Floating a => Vector a -> a
vectLen (Vector x y z) = sqrt ((x) ^ 2 + (y) ^ 2 + (z) ^ 2)

vectSum :: Num a => Vector a -> Vector a -> Vector a
vectSum (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

vectScal :: Num a => Vector a -> Vector a -> a
vectScal (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

vectVect :: Num a => Vector a -> Vector a -> Vector a
vectVect (Vector x1 y1 z1) (Vector x2 y2 z2) =
    Vector (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
