module Fork(foork) where

import System.IO
import Control.Concurrent

-- show
foork :: IO ()
foork = do
    forkIO (hPutStr stdout "Hello")
    hPutStr stdout " world\n"
