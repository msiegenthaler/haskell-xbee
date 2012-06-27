module System.Hardware.XBee.Connector.Common (
    runScheduler
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Hardware.XBee.Device


-- | Handles the xSchedule aspect of the interface. Reads and schedules the items.
--   The scheduler terminates as soon as the stopRequest-TVar is filled.
runScheduler :: XBeeInterface -> TMVar () -> IO ()
runScheduler x stopRequest = readItem >>= handle
    where readItem = atomically $ liftM Right (xSchedule x)
                         `orElse` liftM Left (takeTMVar stopRequest)
          handle (Right s) = forkIO (schedule s) >> runScheduler x stopRequest
          handle (Left _) = return ()
          schedule (Scheduled time action) = threadDelay time >> action

