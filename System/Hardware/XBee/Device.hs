{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module System.Hardware.XBee.Device (
    -- * Device
    XBee,
    newDevice,
    -- * Commands
    FrameCmdSpec(..),
    FramelessCmdSpec(..),
    resultGet,
    sendCommand,
    sendCommandAndWaitIO,
    fireCommand,
    fireCommandIO
) where

import System.Hardware.XBee.Frame
import System.Hardware.XBee.Command
import System.Hardware.XBee.PendingFrames
import Data.Word
import Data.Serialize
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Units
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Timeout
import Data.SouSiT
import qualified Data.SouSiT.Trans as T


-- | Opaque representation of the locally attached XBee device.
data XBee = XBee { sendQueue :: TChan (CommandOut,IO ()),
                   outQueue  :: TChan CommandOut,
                   inQueue   :: TChan CommandIn,
                   threads   :: (ThreadId, ThreadId, ThreadId),
                   pendingFrames :: PendingFrames }


-- | Creates a new XBee device based upon a byte source and sink.
newDevice :: Source src => src IO Word8 -> Sink Word8 IO () -> IO XBee
newDevice src sink = do
        iq <- newTChanIO
        oq <- newTChanIO
        sq <- newTChanIO
        tt <- forkIO $ runTimeouter sq oq
        tr <- forkIO $ runReadIn src iq
        tw <- forkIO $ runWriteOut oq sink
        pf <- atomically newPendingFrames
        return $ XBee sq oq iq (tr,tt,tw) pf

runTimeouter :: TChan (CommandOut,IO ()) -> TChan CommandOut -> IO ()
runTimeouter i o = atomically transfer >>= forkIO >> return ()
    where transfer = do (m, toa) <- readTChan i
                        writeTChan o m
                        return toa

runReadIn :: Source src => src IO Word8 -> (TChan CommandIn) -> IO ()
runReadIn src c = src $$ word8ToFrame =$= T.map frameToCommand =$= T.eitherRight =$ sink
    where sink = tchanSink c

runWriteOut :: (TChan CommandOut) -> Sink Word8 IO () -> IO ()
runWriteOut c sink = src $$ T.map commandToFrame =$= frameToWord8 =$ sink
    where src = tchanSource c

tchanSink :: TChan a -> Sink a IO ()
tchanSink c = SinkCont step (return ())
    where step i = push i >> return (tchanSink c)
          push i = atomically $ writeTChan c i

tchanSource :: TChan a -> BasicSource2 IO a
tchanSource c = BasicSource2 step
    where step (SinkCont next _) = pull >>= next >>= step
          step s@(SinkDone _)    = return s
          pull = atomically $ readTChan c


data FrameCmdSpec a   = FrameCmdSpec (FrameId -> CommandOut) (CommandHandler a)
data FramelessCmdSpec = FramelessCmdSpec CommandOut

-- | Process a command and return a "future" for getting the response.
-- The result is read using resultGet in a different atomically than the sendCommand.
sendCommand :: TimeUnit time => XBee -> FrameCmdSpec a -> time -> STM (XBeeResult a)
sendCommand x (FrameCmdSpec cmd ch) tmo = do
        (fid, r) <- enqueue (pendingFrames x) ch
        writeTChan (sendQueue x) (cmd fid,timeout r)
        return r
    where timeout r = threadDelay (tmous) >> (atomically $ resultTimeout r)
          tmous = fromIntegral $ toMicroseconds tmo

-- | Executes sendCommand and resultGet together.
sendCommandAndWaitIO x cmd t = atomically send >>= atomically . resultGet
    where send = sendCommand x cmd t

-- | Sends a command without checking for a response.
fireCommand :: XBee -> FramelessCmdSpec -> STM ()
fireCommand x (FramelessCmdSpec cmd) = writeTChan (outQueue x) cmd

-- | Executes fireCommand.
fireCommandIO x s = atomically $ fireCommand x s
