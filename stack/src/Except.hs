module Except where

import Control.Monad (ap, liftM)


data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)
newtype Except e a = Except {runExcept :: Either e a}

throwE :: e -> Except e a
throwE = except . Left

tryRead :: Read a => String -> Either String a
tryRead s = case (read s) of
  x -> Right x
  _ -> Left "kek"

except :: Either e a -> Except e a
except = Except

(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) xs i | i < 0           = throwE ErrNegativeIndex
           | otherwise       = helper (take (i+1) xs) where
             helper xs | (i+1) > length xs = throwE $ ErrIndexTooLarge i
                       | otherwise     = except $ Right (last xs)

instance Functor (Except e) where
  fmap = liftM

instance Applicative (Except e) where
  pure = return
  (<*>) = ap

instance Monad (Except e) where
  return = except . Right
  m >>= k = case (runExcept m) of
    (Left e)  -> throwE e
    (Right a) -> k a
