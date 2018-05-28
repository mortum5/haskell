import Control.Concurrent (forkIO, threadDelay, readChan, writeChan, newChan)
import Control.Monad (forever)
import System.Random (randomRIO)

workerCount, workloadCount, minDelay, maxDelay :: Int
workerCount = 250
workloadCount = 10000
minDelay = 250000 -- in microseconds, == 0.25 seconds
maxDelay = 750000 --                  == 0.75 seconds


worker requestChan responseChan workerId = forkIO $ forever $ do
    -- Get a random delay value between the min and max delays
    delay <- randomRIO (minDelay, maxDelay)
    -- Delay this thread by that many microseconds
    threadDelay delay
    -- Read the next item off of the request channel
    int <- readChan requestChan
    -- Write the response to the response channel
    writeChan responseChan (workerId, int * int)

main = do
    -- Create our communication channels
    requestChan <- newChan
    responseChan <- newChan

    -- Spawn off our worker threads. mapM_ performs the given action
    -- on each value in the list, which in this case is the
    -- identifiers for each worker.
    mapM_ (worker requestChan responseChan) [1..workerCount]

    -- Define a helper function to handle each integer in the workload
    let perInteger int = do
            -- Write the current item to the request channel
            writeChan requestChan int
            -- Read the result off of the response channel
            (workerId, square) <- readChan responseChan
            -- Print out a little message
            putStrLn $ concat
                [ "Worker #"
                , show workerId
                , ": square of "
                , show int
                , " is "
                , show square
                ]

    -- Now let's loop over all of the integers in our workload
    mapM_ perInteger [1..workloadCount]
