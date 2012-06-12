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
    fireCommandIO,
    -- * Source
    rawInSource,
    rawInSourceSTM,
    rawInSourceIO,
    ReceivedMessage(..),
    messagesSource,
    messagesSourceSTM,
    messagesSourceIO,
    modemStatusSource,
    modemStatusSourceSTM,
    modemStatusSourceIO
) where

import System.Hardware.XBee.Frame
import System.Hardware.XBee.Command
import System.Hardware.XBee.PendingFrames
import Data.List
import Data.Word
import Data.Time.Units
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.SouSiT
import qualified Data.SouSiT.Trans as T


-- | Opaque representation of the locally attached XBee device.
data XBee = XBee { sendQueue :: TChan (CommandOut,IO ()),
                   outQueue  :: TChan CommandOut,
                   subs      :: TChan CommandIn,
                   threads   :: (ThreadId, ThreadId, ThreadId),
                   pendingFrames :: PendingFrames }


-- | Creates a new XBee device based upon a byte source and sink.
newDevice :: Source src => src IO Word8 -> Sink Word8 IO () -> IO XBee
newDevice src sink = do
        oq <- newTChanIO
        sq <- newTChanIO
        ss <- newTChanIO
        pf <- atomically newPendingFrames
        tt <- forkIO $ runTimeouter sq oq
        tr <- forkIO $ runReadIn src pf ss
        tw <- forkIO $ runWriteOut oq sink
        return $ XBee sq oq ss (tr,tt,tw) pf
-- TODO exception handling!

runTimeouter :: TChan (CommandOut,IO ()) -> TChan CommandOut -> IO ()
runTimeouter i o = atomically transfer >>= forkIO >> return ()
    where transfer = do (m, toa) <- readTChan i
                        writeTChan o m
                        return toa

runReadIn :: Source src => src IO Word8 -> PendingFrames -> TChan CommandIn -> IO ()
runReadIn src pf subs = src $$ word8ToFrame =$= T.map frameToCommand =$= T.eitherRight =$ sink
    where sink = stmSink (processIn pf subs)

processIn :: PendingFrames -> TChan CommandIn -> CommandIn -> STM ()
processIn pf subs msg = handleCommand pf msg >> writeTChan subs msg

runWriteOut :: (TChan CommandOut) -> Sink Word8 IO () -> IO ()
runWriteOut c sink = src $$ T.map commandToFrame =$= frameToWord8 =$ sink
    where src = tchanSource c

stmSink :: (a -> STM ()) -> Sink a IO ()
stmSink f = SinkCont step (return ())
    where step i = atomically (f i) >> return (stmSink f)

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



-- | Source for all incoming commands from the XBee. This includes replies to framed command
-- that are also handled by sendCommand.
rawInSource :: Monad m => (forall x . STM x -> m x) -> XBee -> BasicSource2 m CommandIn
rawInSource mf x = BasicSource2 first
    where first sink = mf (dupTChan (subs x)) >>= (flip step) sink
          step c (SinkCont next _) = mf (readTChan c) >>= next >>= step c
          step c done = return done

-- | STM version of rawInSource.
rawInSourceSTM = rawInSource id

-- | IO version of rawInSource.
rawInSourceIO = rawInSource atomically


-- | An incoming message (abstracts over Receive64 and Receive16)
data ReceivedMessage = ReceivedMessage { sender :: XBeeAddress,
                                         signal :: SignalStrength,
                                         addressBroadcast :: Bool,
                                         panBroadcast :: Bool,
                                         messageBody :: [Word8] }

-- Transformer for CommandIn into ReceivedMessage.
commandInToReceivedMessage :: Transform CommandIn ReceivedMessage
commandInToReceivedMessage = T.filterMap step
    where step (Receive16 se si ab pb d) = Just $ ReceivedMessage (XBeeAddress16 se) si ab pb d
          step (Receive64 se si ab pb d) = Just $ ReceivedMessage (XBeeAddress64 se) si ab pb d
          step _ = Nothing

-- | Source for all messages received from remote XBees (Receive16 and Receive64).
messagesSource :: Monad m => (forall x . STM x -> m x) -> XBee -> BasicSource m ReceivedMessage
messagesSource mf x = rawInSource mf x $= commandInToReceivedMessage

-- | STM version of messagesSource
messagesSourceSTM = messagesSource id

-- | IO version of messagesSource
messagesSourceIO = messagesSource atomically


-- | Source for modem status updates.
modemStatusSource :: Monad m => (forall x . STM x -> m x) -> XBee -> BasicSource m ModemStatus
modemStatusSource mf x = rawInSource mf x $= commandInToModemStatus

commandInToModemStatus :: Transform CommandIn ModemStatus
commandInToModemStatus = T.filterMap step
    where step (ModemStatusUpdate s) = Just s
          step _ = Nothing

-- | STM version of modemStatusSource
modemStatusSourceSTM = modemStatusSource id

-- | IO version of modemStatusSource
modemStatusSourceIO = modemStatusSource atomically
