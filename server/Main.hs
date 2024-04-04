module Main where

-- Compile with -threaded

import Control.Concurrent
import GHC.Base (($!))
import System.IO

getGreeting :: IO String
getGreeting = do
  -- Get id and convert to string
  tid <- myThreadId
  let greeting = "Hello from " ++ show tid
  -- Force evaluation of greeting and return
  return $! greeting

threadHello :: MVar () -> Chan () -> IO ()
threadHello mutex endFlags = do
  -- Compute greeting (finished before getting mutex)
  greeting <- getGreeting
  -- Get mutex (acquires lock for output)
  takeMVar mutex
  -- Say hello
  putStrLn greeting
  -- Release mutex (give up lock, another thread can take over)
  putMVar mutex ()
  -- Signal end of thread
  writeChan endFlags ()

main :: IO ()
main = do
  -- Disable buffering on stdout
  hSetBuffering stdout NoBuffering
  -- Number of threads to spawn
  let n = 10
  -- Init mutex and FIFO for end flags
  mutex <- newEmptyMVar
  endFlags <- newChan
  -- Spawn threads (threads are waiting for mutex before printing)
  mapM_ (const $ forkIO $ threadHello mutex endFlags) [(1 :: Int) .. n]
  -- Give mutex its value (threads start aquiring mutex here)
  putMVar mutex ()
  -- Read n end flags (blocks until all threads have sent their end signal)
  mapM_ (const $ readChan endFlags) [1 .. n]