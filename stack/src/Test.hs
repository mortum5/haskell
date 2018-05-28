module Test where

coins :: (Num a) => [a]
coins = [2, 3, 7]



newtype ZipList a = ZipList { getZipList :: [a] } deriving Show

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList gs <*> ZipList xs = ZipList (zipWith ($) gs xs)

infixl 4 >$<
(>$<) f xs = getZipList $ f <$> (ZipList xs)

infixl 4 >*<
(>*<) f xs = getZipList $ (ZipList f) <*> (ZipList xs)
