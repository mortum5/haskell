import Data.List
import Control.Monad

solveRPN :: (Num a, Read a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction xs numberString = read numberString:xs  

solveRPNwithST :: String -> Maybe Double
solveRPNwithST st = do
    [result] <- foldM foldingFunctionST [] (words st)
    return result
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing
foldingFunctionST :: [Double] -> String -> Maybe [Double]  
foldingFunctionST (x:y:ys) "*" = return ((x * y):ys)  
foldingFunctionST (x:y:ys) "+" = return ((x + y):ys)  
foldingFunctionST (x:y:ys) "-" = return ((y - x):ys)  
foldingFunctionST xs numberString = liftM (:xs) (readMaybe numberString) 
