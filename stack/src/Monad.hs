module Monad where

import Control.Monad.Writer
import Control.Monad.Reader

type MyRW = ReaderT [String] (Writer String)

import Data.List
separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate f s xs = do
    lift $ tell ss
    tell fs
    xsss <- lift $ return xxs
    return xsss
        where
            fs = fst $ partition f xs
            ss = fst $ partition s xs
            xxs = (snd . partition s . snd . partition f) xs
