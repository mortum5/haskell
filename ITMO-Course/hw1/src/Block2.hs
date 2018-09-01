module Block2
    ( stringSum
    , mergeSort
    , collectEvery
    , removeAt
    ) where

stringToInt :: String -> Int
stringToInt = read

stringSum :: String -> Int
stringSum x = sum $ map (stringToInt . filter ((`notElem` "+"))) $ words x

mergeSort :: Ord t => [t] -> [t]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort as) (mergeSort bs)
  where
    (as, bs) = splitInHalf xs
    splitInHalf :: [t] -> ([t], [t])
    splitInHalf [] = ([], [])
    splitInHalf [x] = ([x], [])
    splitInHalf (x:y:xys) = (x : xs, y : ys)
      where
        (xs, ys) = splitInHalf xys -- Or splitAt length / 2

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) =
    if x < y
        then x : merge xs (y : ys)
        else y : merge ys (x : xs)

removeAt :: Int -> [a] -> [a]
removeAt _ []     = []
removeAt 0 (_:xs) = xs
removeAt i (x:xs) = x : removeAt (i - 1) xs

collectEvery :: Int -> [a] -> ([a], [a])
collectEvery x p = collect 1 p [] []
  where
    collect _ [] l r = (reverse l, reverse r)
    collect i (y:ys) l r
        | i == x = collect 1 ys l (y : r)
        | otherwise = collect (i + 1) ys (y : l) r
