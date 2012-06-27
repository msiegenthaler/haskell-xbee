module System.Hardware.XBee.Connector.Common (
    timeoutSink
) where

import Data.SouSiT
import Control.Concurrent
import Control.Monad
import System.Hardware.XBee.Device


-- | Sink that schedules the received. The sink is never done.
--   Pending timeouts are processed even after the sink is closed.
timeoutSink :: Sink Scheduled IO ()
timeoutSink = SinkCont next done
    where next i = (return i) >>= forkIO . schedule >> return (SinkCont next done)
          done = return ()
          schedule (Scheduled time action) = threadDelay time >> action
