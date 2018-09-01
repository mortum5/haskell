module SplitOn
    ( splitOn
    ) where

splitOn :: Foldable t => Char -> t Char -> [[Char]]
splitOn t l =
    foldr
        (\x (n:ns) ->
             if (x == t)
                 then ("" : n : ns)
                 else (x : n) : ns)
        [""]
        l
