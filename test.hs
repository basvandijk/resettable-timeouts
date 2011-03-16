{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.Event (EventManager)
import GHC.Conc.Sync (sharedCAF)
import Foreign.Ptr (Ptr)
import Data.IORef (IORef, newIORef, writeIORef, readIORef, atomicModifyIORef)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad

import Control.Concurrent

import System.Timeout.Resetable.ADT

--------------------------------------------------------------------------------

main = do 
  Just mgr <- readIORef eventManager
  k <- register mgr 500000 $ putStrLn "Timeout!"

  pause k

  replicateM_ 10 $ do
    threadDelay 100000
    reset k 500000

  cancel mgr k

  threadDelay 1000000

--------------------------------------------------------------------------------

{-# NOINLINE eventManager #-}
eventManager :: IORef (Maybe EventManager)
eventManager = unsafePerformIO $ do
                 em <- newIORef Nothing
                 sharedCAF em getOrSetSystemEventThreadEventManagerStore

foreign import ccall unsafe "getOrSetSystemEventThreadEventManagerStore"
    getOrSetSystemEventThreadEventManagerStore :: Ptr a -> IO (Ptr a)

-- The End ---------------------------------------------------------------------
