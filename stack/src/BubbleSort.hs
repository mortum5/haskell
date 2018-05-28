module BubbleSort(bubbleSort, sort, sort', parse) where

import System.IO
import Data.Char

bubbleSort :: IO ()
bubbleSort = do
  hI <- openFile "input.txt" ReadMode
  hO <- openFile "output.txt" WriteMode
  buf <- hGetContents hI
  hPutStrLn hO (parse buf)
  hClose hI
  hClose hO



parse s = show $ sort' (length l) l   where
            l = ((map read) . words . head . lines $ s :: [Int])

sort' 0 l = l
sort' n l = sort' (n-1) (sort [] l)

sort x [] = reverse x
sort x ([y]) = sort (y:x) []
sort ys (x:y:xs) | (x > y) = sort (y:ys) (x:xs)
                 | otherwise = sort (x:ys) (y:xs)
