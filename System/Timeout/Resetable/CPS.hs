module System.Timeout.Resetable.CPS
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
-- Resetable timeouts
---------------------------------------------------------------------

data Key = Key !EventManager !TimeoutKey !(IORef OnTimeout)

type OnTimeout = Int -> TimeoutCallback -> TimeoutKey -> IO ()

register :: EventManager -> Int -> TimeoutCallback -> IO Key
register mgr us cb = do
  ref <- newIORef callCallback
  tk <- newTimeoutKey mgr
  continue mgr ref us cb tk
  return $ Key mgr tk ref

callCallback :: OnTimeout
callCallback _ cb _ = cb

continue :: EventManager -> IORef OnTimeout -> OnTimeout
continue mgr ref = \us cb tk -> insertTimeout mgr tk us $ 
                                  readIORef ref >>= \onTimeout -> 
                                    onTimeout us cb tk

pause :: Key -> IO ()
pause (Key mgr _ ref) = writeIORef ref $ continue mgr ref

reset :: Key -> IO ()
reset (Key mgr _ ref) = writeIORef ref $ \us cb tk ->
                          writeIORef ref callCallback >>
                          continue mgr ref us cb tk

whilePaused :: Key -> IO a -> IO a
whilePaused key = bracket_ (pause key) (reset key)

cancel :: Key -> IO ()
cancel (Key mgr tk _) = unregisterTimeout mgr tk

-- The End ----------------------------------------------------------
