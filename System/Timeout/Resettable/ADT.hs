module System.Timeout.Resettable.ADT
    ( TimeoutCallback
    , Key
    , register
    , pause
    , reset
    , whilePaused
    , cancel
    ) where

---------------------------------------------------------------------
-- Imports
---------------------------------------------------------------------

import Control.Exception ( bracket_ )
import Data.IORef        ( IORef, newIORef
                         , readIORef, writeIORef
                         )
import System.Event      ( EventManager
                         , TimeoutCallback
                         , TimeoutKey
                         , newTimeoutKey
                         , insertTimeout
                         , unregisterTimeout
                         )

---------------------------------------------------------------------
-- Resettable timeouts
---------------------------------------------------------------------

data Key = Key !EventManager !TimeoutKey !(IORef State) 

data State = Active | Reset | Pause

register :: EventManager -> Int -> TimeoutCallback -> IO Key
register mgr us cb = do
  ref <- newIORef Active
  tk <- newTimeoutKey mgr
  let loop = insertTimeout mgr tk us $
               readIORef ref >>= \state ->
                 case state of
                   Active -> cb
                   Reset  -> writeIORef ref Active >> loop
                   Pause  -> loop
  loop
  return $ Key mgr tk ref

pause :: Key -> IO ()
pause (Key _ _ ref) = writeIORef ref Pause

reset :: Key -> IO ()
reset (Key _ _ ref) = writeIORef ref Reset

whilePaused :: Key -> IO a -> IO a
whilePaused (Key _ _ ref) = bracket_ (writeIORef ref Pause)
                                     (writeIORef ref Reset)

cancel :: Key -> IO ()
cancel (Key mgr tk _) = unregisterTimeout mgr tk

-- The End ----------------------------------------------------------
