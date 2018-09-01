module FTP where

import           Data.Monoid (mempty, (<>))

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a)
         a
         (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldr _ z Empty        = z
  foldr f z (Leaf a)     = f z a
  foldr f z (Node l x r) = foldr f (f z (foldr z l)) r -- Where x??? Why foldr z l, where f??
  foldMap _ Empty        = mempty
  foldMap f (Leaf a)     = f a
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Traversable Tree where
  traverse _ Empty        = pure Empty
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r
