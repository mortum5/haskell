module Eval where

import Control.Monad
import qualified Data.Map as M(Map, lookup, fromList)
import Control.Applicative
data Exp = Var String | Lit Int | Neg Exp | Add Exp Exp | Mul Exp Exp deriving (Show, Eq)

data Reader env a = Reader (env -> a)

runReader :: Reader env a -> env -> a
runReader (Reader f) = f

instance Monad (Reader env) where
    return a    = Reader $ const a
    ma >>= mf   = Reader $ \env -> 
                    let b = runReader ma env
                    in  runReader (mf b) env 
                    
instance Applicative (Reader env) where
    pure = return
    (<*>) = ap

instance Functor (Reader env) where
    fmap = liftM
instance Num Exp where
    negate = Neg
    (+) = Add
    (*) = Mul

    fromInteger = Lit . fromInteger

    abs = undefined
    signum = undefined

var :: String -> Exp
var = Var

n :: Int -> Exp
n = var . show

eval :: Exp -> Reader Env Int
eval (Lit n)  = pure n
eval (Neg n)  = negateA $ eval n
eval (Add a b) = eval a `addA` eval b
eval (Mul a b) = eval a `mulA` eval b
eval (Var name) = Reader $ \env -> value env name

addA = liftA2 (+)
mulA = liftA2 (*)
negateA = liftA negate

type Env = M.Map String Int

value :: Env -> String -> Int
value env name = maybe errorMsg $ M.lookup env name
    where errorMsg = error $ "value is undefined for " ++ name

runExp :: Exp -> [(String, Int)] -> Int
runExp a env = runReader (eval a) $ M.fromList env
