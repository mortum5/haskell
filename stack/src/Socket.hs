module Socket(main') where

import Network.Socket
import Control.Monad
import Control.Concurrent
import System.IO

main' :: IO ()
main' = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY )
  listen sock 5
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  forkIO $ talk (fst conn)
  mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Hello!\n"
    close sock

talk :: Socket -> IO ()
talk sock = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  buf 
  hClose hdl
