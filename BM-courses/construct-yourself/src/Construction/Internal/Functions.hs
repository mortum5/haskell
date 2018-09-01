{-# LANGUAGE RecordWildCards #-}
                                 -- they make your code clean and clear.
                                 -- Read about this extension here:
                                 -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html



module Construction.Internal.Functions
  ( Context (..)        -- make restrictions is good practice. As you can see here,
  , fresh, free, bound  -- we make "public" not all functions, but only Context, fresh, ...
  , reduce, substitute, alpha, beta, eta
  )where

import           Construction.Internal.Types (Name, Term (..))
import           Data.Set                    (Set, delete, empty, insert,
                                              member, notMember, singleton,
                                              union)
import           Data.Text                   (pack)


-- Context is just set of names that are in our context.
type Context = Set Name

-- | @fresh@ generates new variable different from every variables in set.
fresh :: Set Name -> Name
fresh conflicts = head . dropWhile (`member` conflicts) $ nameGen -- This is ugly name generator. Make it better.
  where nameGen = [pack $ 'x' : show ind | ind <- [0..] :: [Int]]

-- | @free@ finds all free (Amazing!) variables from given term.
free :: Term -> Set Name
free (Var var)           = singleton var
free (App algo arg)      = free algo `union` free arg
free (Lam variable body) = variable `delete` free body

-- | @bound@ finds all bounded variables from given term.
-- This function uses RecordWildCards.
-- If you like it refactor @free@ function.
bound :: Term -> Set Name
bound Var{}   = empty
bound App{..} = bound algo `union` bound arg
bound Lam{..} = variable `insert` bound body

-- a[n := b] - substiturion
substitute :: Term -> Name -> Term -> Term
substitute v@Var{..} n b | var == n  = b
                         | otherwise = v
substitute a@App{..} n b = App (substitute algo n b) (substitute arg n b)
substitute l@Lam{..} n b
    | n == variable = l
    | otherwise =
        if variable `member` free b then substitute (alpha l (singleton variable)) n b
        else Lam variable (substitute body n b)

-- | alpha reduction
alpha :: Term -> Set Name -> Term
alpha t s = alphaDeep t s empty

alphaDeep :: Term -> Set Name -> Set Name-> Term
alphaDeep v@Var{..} s p
    | var `member` s && var `member` p = Var (fresh s)
    | otherwise = v
alphaDeep App{..} s p = App (alphaDeep algo s p) (alphaDeep arg s p)
alphaDeep l@Lam{..} s p
    | variable `member` s = Lam (fresh s) (alphaDeep body s (variable `insert` p))
    | otherwise = Lam variable (alphaDeep body s p)

-- | beta reduction
beta :: Term -> Term
beta v@Var{..}                 = v
beta (App (Lam name body) arg) = substitute body name arg
beta App{..}                   = App (beta algo) (beta arg)
beta Lam{..}                   = Lam variable (beta body)

-- | eta reduction
eta :: Term -> Term
eta v@Var{..} = v
eta a@App{..} = a
eta l@(Lam var (App x (Var name)))
    | name == var && not(name `member` free(x)) = x
    | otherwise = l
eta l@(Lam var _) = l

-- | reduce term
reduce :: Term -> Term
reduce term = let term' = beta term
              in if term' == term
                 then eta term
                 else reduce term'
