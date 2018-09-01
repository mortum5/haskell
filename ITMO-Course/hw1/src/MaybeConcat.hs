module MaybeConcat
    ( maybeConcat
    ) where

maybeConcat :: [Maybe [t]] -> [t]
maybeConcat [] = []
maybeConcat (x:xs) = helper x ++ maybeConcat xs
  where
    helper (Just x)  = x
    helper (Nothing) = []
