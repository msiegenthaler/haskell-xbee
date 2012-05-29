{-# LANGUAGE RankNTypes #-}

module System.Hardware.XBee.Device (
    -- * Device
    XBee,
    newDevice
) where

import System.Hardware.XBee.Frame
import System.Hardware.XBee.Command
import Data.Word
import Data.Serialize
import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.SouSiT
import qualified Data.SouSiT.Trans as T


data XBee = XBee (TChan CommandIn) (TChan CommandOut) [ThreadId]

-- | Creates a new XBee device based upon a byte source and sink.
newDevice :: Source src => src IO Word8 -> Sink Word8 IO () -> IO XBee
newDevice src sink = do
        inQ  <- newTChanIO
        outQ <- newTChanIO
        r <- forkReader src' inQ
        w <- forkWriter sink' outQ
        return $ XBee inQ outQ [r,w]
    where sink' = T.map commandToFrame =$= frameToWord8 =$ sink
          src'  = src $= word8ToFrame =$= T.map frameToCommand =$= T.eitherRight
-- TODO Exception handling!

forkReader :: Source src => src IO a -> TChan a -> IO ThreadId
forkReader src c = forkIO body
    where body = src $$ (tchanSink c)

forkWriter :: Sink a IO () -> TChan a -> IO ThreadId
forkWriter sink c = forkIO body
    where body = (tchanSource c) $$ sink

tchanSink :: TChan a -> Sink a IO ()
tchanSink c = SinkCont step (return ())
    where step i = push i >> return (tchanSink c)
          push i = atomically $ writeTChan c i

tchanSource :: TChan a -> BasicSource2 IO a
tchanSource c = BasicSource2 step
    where step (SinkCont next _) = pull >>= next >>= step
          step s@(SinkDone _)    = return s
          pull = atomically $ readTChan c
