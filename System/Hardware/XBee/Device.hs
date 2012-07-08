{-# LANGUAGE RankNTypes #-}

module System.Hardware.XBee.Device (
    -- * Device
    XBee,
    newDevice,
    -- * Device Interface
    XBeeInterface(..),
    Scheduled(..),
    -- * Commands
    -- ** Types
    CommandHandler,
    fetch,
    CommandResponse(..),
    FrameCmd(..),
    -- ** Functions
    sendCommand,
    fireCommand,
    -- * Source
    rawInSource,
    ReceivedMessage(..),
    messagesSource,
    modemStatusSource,
) where

import System.Hardware.XBee.Frame
import System.Hardware.XBee.Command
import Data.List
import Data.Word
import Data.Time.Units
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative
import Control.RequestResponseCorrelator
import Data.SouSiT
import Data.SouSiT.STM
import qualified Data.SouSiT.Trans as T


-- | Answers received to a command sent to the XBee.
data CommandResponse = CRData CommandIn
                     | CRPurged
                     | CRTimeout deriving (Show, Eq)

-- | Handler for the answers to a single command sent to the XBee.
type CommandHandler a = ResponseM CommandResponse a

-- | A command that expects an answer from the XBee.
data FrameCmd a   = FrameCmd (FrameId -> CommandOut) (CommandHandler a)
instance Functor FrameCmd where
    fmap f (FrameCmd a handler) = FrameCmd a (fmap f handler)



-- | Task to be scheduled. Delay in microseconds and the action to execute after the delay.
data Scheduled = Scheduled Int (IO ())

type XBeeCorrelator =  Correlator FrameId CommandResponse

-- | Opaque representation of the locally attached XBee device.
data XBee = XBee { scheduleQueue :: TChan Scheduled,
                   outQueue  :: TChan CommandOut,
                   inQueue   :: TChan CommandIn,
                   correlator :: XBeeCorrelator }


-- | Interface to an XBee. This is 'side' of the xbee that needs to be attached to the
--   actual device. See i.e. the HandleDeviceConnector module.
data XBeeInterface = XBeeInterface {
                    -- | Commands that are sent to the xbee device (serial port).
                    outgoing   :: BasicSource2 IO CommandOut,
                    -- | Commands received from the xbee device (serial port).
                    incoming   :: Sink CommandIn IO (),
                    -- | Actions to be scheduled with the specified delay.
                    --   This is used mainly for timeouts.
                    toSchedule :: BasicSource2 IO Scheduled }


-- | Create a new XBee device along with the corresponding interface.
newDevice :: STM (XBee, XBeeInterface)
newDevice = do
        inQ  <- newTChan
        outQ <- newTChan
        scdQ <- newTChan
        corr <- newCorrelator CRPurged
        let xbee = XBee scdQ outQ inQ corr
        let xif  = mkXif inQ outQ scdQ corr
        return (xbee, xif)

mkXif inQ outQ scdQ corr = XBeeInterface o i s
    where o = tchanSource outQ
          s = tchanSource scdQ
          i = stmSink' $ processIn corr inQ

processIn :: XBeeCorrelator -> TChan CommandIn -> CommandIn -> STM ()
processIn corr subs msg = handle (frameIdFor msg) >> writeTChan subs msg
    where handle (Just frame) = push corr frame (CRData msg)
          handle Nothing = return ()


-- | Process a command and return a "future" for getting the response.
sendCommand :: TimeUnit time => XBee -> time -> FrameCmd a -> STM (STM a)
sendCommand x tmo (FrameCmd cmd h) = do
        (frame, future, feedFun) <- request (correlator x) h
        writeTChan (outQueue x) (cmd frame)
        writeTChan (scheduleQueue x) $ Scheduled tmoUs (atomically $ feedFun CRTimeout)
        return future
    where tmoUs = fromIntegral $ toMicroseconds tmo

-- | Sends a command without checking for a response.
fireCommand :: XBee -> CommandOut -> STM ()
fireCommand x cmd = writeTChan (outQueue x) cmd



-- | Source for all incoming commands from the XBee. This includes replies to framed command
-- that are also handled by sendCommand.
rawInSource :: XBee -> BasicSource2 IO CommandIn
rawInSource x = BasicSource2 first
    where first sink = atomically (dupTChan (inQueue x)) >>= (flip step) sink
          step c (SinkCont next _) = atomically (readTChan c) >>= next >>= step c
          step c done = return done

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
messagesSource :: XBee -> BasicSource IO ReceivedMessage
messagesSource x = rawInSource x $= commandInToReceivedMessage

-- | Source for modem status updates.
modemStatusSource :: XBee -> BasicSource IO ModemStatus
modemStatusSource x = rawInSource x $= commandInToModemStatus

commandInToModemStatus :: Transform CommandIn ModemStatus
commandInToModemStatus = T.filterMap step
    where step (ModemStatusUpdate s) = Just s
          step _ = Nothing
