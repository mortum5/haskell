module Block1
    ( order3
    , highestBit
    , smartReplicate
    , contains
    ) where

order3 :: (Ord a) => (a, a, a) -> (a, a, a)
order3 (a, b, c) =
    let f = cmp a b
        s = cmp (snd f) c
    in (fst f, fst s, snd s)
  where
    cmp :: (Ord a) => a -> a -> (a, a)
    cmp x y
        | x > y = (y, x)
        | otherwise = (x, y)

highestBit :: (Ord t2, Num t2, Num t1, Integral t) => t2 -> (t1, t)
highestBit x =
    let t = helper x 1 0
    in (2 ^ t, t)
  where
    helper x p c
        | (p < x) = helper x (p * 2) (c + 1)
        | (p == x) = c
        | otherwise = c - 1

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

contains :: (Eq a) => a -> [[a]] -> [[a]]
contains x = filter (elem x)
