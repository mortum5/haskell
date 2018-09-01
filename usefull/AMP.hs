{-# LANGUAGE TemplateHaskell #-}

module AFM where

class Functor f where
  fmap :: (a -> b) -> f a -> f b

fmap id x == x

fmap f . fmap g == fmap (f . g)

class Functor f =>
      Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b -- LiftA

fmap f x == liftA f x

liftA id x == x

liftA3 (.) f g x == f <*> (g <*> x)

liftA f (pure x) == pure (f x)

data State s a =
  State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

data Writer msg b =
  Writer (b, msg)

runWriter :: Write msg b -> (b, msg)
runWriter (Writer a) = a

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> ma
  fail :: String -> m a
--  return a >>= k    = k a
--    m >>= return
--  m >>= (\x -> k x >>= h) = (m >>= k) >>= h
