module Lib
    ( someFunc,
      todoList,
      sequence_',
      test,
      bubbleSort,
      concur
    ) where

import Control.Monad
import qualified Data.Vector as V
import Control.Concurrent
import Data.IORef
someFunc :: IO ()
someFunc = putStrLn "someFunc"

todoList :: [IO ()]




test = do
  xs <- mapM newIORef [1,2]
  let ix = xs !! 0
  let iy = xs !! 1
  x <- readIORef ix
  y <- readIORef iy
  writeIORef ix y
  writeIORef iy x
  return xs


concur = do
    a <- newEmptyMVar

    forkIO $ forever $ takeMVar a >>= putStrLn

    forever $ do
        text <- getLine
        putMVar a text

todoList = [putChar 'a',
            do putChar 'b'
               putChar 'c',
            do c <- getChar
               putChar c]


sequence_'        :: [IO ()] -> IO ()
sequence_'        =  foldr (>>) (return ())

bubbleSort :: [Int] -> IO [Int]
bubbleSort input = do
    let ln = length input

    xs <- mapM newIORef input

    forM_ [0..ln - 1] $ \_ -> do
        forM_ [0..ln - 2] $ \j -> do
            let ix = xs !! j
            let iy = xs !! (j + 1)

            x <- readIORef ix
            y <- readIORef iy

            when (x > y) $ do
                writeIORef ix y
                writeIORef iy x

    mapM readIORef xs
