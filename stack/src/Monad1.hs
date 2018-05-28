module Monad1
    (count,
    listDirectory,
    pro,
    mon,
    main1,
    runApp,
    constrainedCount
    ) where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)

data AppLog = AppLog {
      filePath :: String,
      counts :: Int
    } deriving (Show)

type App = ReaderT AppConfig (WriterT [AppLog] (StateT AppState IO))


runApp a maxDepth =
  let cfg = AppConfig (maxDepth)
      state = AppState 0
      in runStateT (runWriterT (runReaderT a cfg)) state

constrainedCount :: Int -> FilePath -> App ()
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  tell [AppLog {filePath = path, counts = length contents}]
  cfg <- ask
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do
        let newDepth = curDepth + 1
        st <- get
        when (stDeepestReached st < newDepth) $
          put st {stDeepestReached = newDepth}
        constrainedCount newDepth newPath
    else return ()
  return ()

listDirectory :: FilePath -> IO [FilePath]
listDirectory = liftM (filter dots) . getDirectoryContents where
  dots p = p /= "." && p /= ".."

count :: FilePath -> WriterT [(FilePath, Int)] IO ()
count path = do
  contents <- liftIO . listDirectory $ path
  tell [(path,length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ count newName

mon :: ReaderT Int (WriterT String (StateT Int IO )) ()
mon = do
  e <- ask
  tell (show e)
  put e



main1 :: StateT Int IO ()
main1 = do
  put 1
  e <- get
  liftIO $ putStrLn (show e)


stuff :: Reader Int String
stuff = do
  s <- ask
  return (show s ++ " green bottles")

pro = print $ runReader stuff 99


logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)
