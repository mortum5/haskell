{-# LANGUAGE DeriveFunctor #-}
module Tree
    ( directoryPrint
    , verticalPrint
    , Tree(..)
    , FoldTree(..)
    , find
    , isEmpty
    , count
    , insert
    , delete
    , fromList
    , toList
    ) where

import           TreePrinters (Tree (..), directoryPrint, verticalPrint)

data FoldTree a = Empty | Tree a (FoldTree a) (FoldTree a) deriving (Show, Functor)

instance Foldable FoldTree where
    foldMap _ Empty        = mempty
    foldMap f (Tree l k r) = foldMap f k `mappend` f l `mappend` foldMap f r
    foldr _ z Empty        = z
    foldr f z (Tree l k r) = foldr f (f l (foldr f z r)) k

find :: Ord a => Tree a -> a -> Bool
find Leaf _ = False
find (Node x y z) v
    | x == v = True
    | x > v = find y v
    | x < v = find z v
find Node{} _ = False

isEmpty :: Tree t -> Bool
isEmpty Leaf = True
isEmpty _    = False

count :: Tree a -> Int
count t = length $ toList t

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf x = Node x Leaf Leaf
insert (Node x y z) v
    | x == v = Node x y z
    | v < x = Node x (insert y v) z
    | otherwise = Node x y (insert z v)

delete :: (Ord a) => Tree a -> a -> Tree a
delete Leaf _ = Leaf
delete as@(Node x y z) v
    | v < x = Node x (delete y v) z
    | v > x = Node x y (delete z v)
    | x == v = helper as
delete Node{} _ = Node{}

helper :: Ord t => Tree t -> Tree t
helper (Node _ Leaf z) = z
helper (Node _ y Leaf) = y
helper (Node _ y z) = Node (p z) y (delete z (p z))
  where
    p :: Tree t -> t
    p = findNext

findNext :: Tree t -> t
findNext (Node x Leaf _) = x
findNext (Node _ y _)    = findNext y

toList :: Tree a -> [a]
toList Leaf         = []
toList (Node x y z) = x : (toList y ++ toList z)

fromList :: (Ord a) => [a] -> Tree a -- Why we need Ord there?
fromList x = build x Leaf

build :: (Ord a, Foldable t) => t a -> Tree a -> Tree a
build vs t = foldl insert t vs
