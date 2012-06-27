{-# LANGUAGE RankNTypes #-}

module System.Hardware.XBee.Device (
    -- * Device
    XBee,
    XBeeM(..),
    newDevice,
    -- * Device Interface
    XBeeInterface,
    xWrite,
    xRead,
    xSchedule,
    Scheduled,
    -- * Commands
    FrameCmdSpec(..),
    FramelessCmdSpec(..),
    XBeeResult,
    resultGet,
    sendCommand,
    sendCommandAndWaitIO,
    fireCommand,
    -- * Source
    rawInSource,
    ReceivedMessage(..),
    messagesSource,
    modemStatusSource,
) where

import System.Hardware.XBee.Frame
import System.Hardware.XBee.Command
import System.Hardware.XBee.PendingFrames
import System.Hardware.XBee.DeviceCommand
import Data.List
import Data.Word
import Data.Time.Units
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative
import Data.SouSiT
import qualified Data.SouSiT.Trans as T


-- | Task to be scheduled. Delay in microseconds and the action to execute after the delay.
data Scheduled = Scheduled Int (IO ())

-- | Opaque representation of the locally attached XBee device.
data XBee = XBee { timeoutQueue :: TChan Scheduled,
                   outQueue  :: TChan CommandOut,
                   inQueue   :: TChan CommandIn,
                   pendingFrames :: PendingFrames }

-- | Monad class required interacting with the XBee.
class Monad m => XBeeM m where
    execSTM :: STM a -> m a
instance XBeeM STM where
    execSTM = id
instance XBeeM IO where
    execSTM = atomically

-- | Interface to an XBee. This is 'side' of the xbee that needs to be attached to the
--   actual device. See i.e. the HandleDeviceConnector module.
newtype XBeeInterface = XBeeInterface XBee

unwrap (XBeeInterface x) = x

-- | Pass a command to the interface.
--   Typically this is implemented by reading from the serial port and calling this method.
xWrite :: XBeeInterface -> CommandIn -> STM ()
xWrite x = writeTChan q
    where q = inQueue $ unwrap x

-- | Read a command from the interface.
---  Typically this is implemented by writing the result of this method to the serial port.
xRead :: XBeeInterface -> STM CommandOut
xRead x = readTChan q
    where q = outQueue $ unwrap x

-- | Get the next command to be scheduled from the interface. This is mainly used for timeouts.
--   The connector implementation needs to execute the scheduled items after the specified
--   delay.
xSchedule :: XBeeInterface -> STM Scheduled
xSchedule x = readTChan q
    where q = timeoutQueue $ unwrap x



-- | Create a new XBee device along with the corresponding interface.
newDevice :: STM (XBee, XBeeInterface)
newDevice = liftM both (XBee <$> newTChan <*> newTChan <*> newTChan <*> newPendingFrames)
    where both x = (x, XBeeInterface x)




-- | Process a command and return a "future" for getting the response.
-- The result is read using resultGet in a different atomically than the sendCommand.
sendCommand :: (XBeeM m, TimeUnit time) => XBee -> FrameCmdSpec a -> time -> m (XBeeResult a)
sendCommand x (FrameCmdSpec cmd ch) tmo = execSTM $ do
        (fid, r) <- enqueue (pendingFrames x) ch
        writeTChan (outQueue x) (cmd fid)
        writeTChan (timeoutQueue x) $ Scheduled tmoUs (atomically $ resultTimeout r)
        return r
    where tmoUs = fromIntegral $ toMicroseconds tmo

-- | Executes sendCommand and resultGet together (in two atomicallies).
sendCommandAndWaitIO x cmd t = atomically send >>= atomically . resultGet
    where send = sendCommand x cmd t

-- | Sends a command without checking for a response.
fireCommand :: XBeeM m => XBee -> FramelessCmdSpec -> m ()
fireCommand x (FramelessCmdSpec cmd) = execSTM $ writeTChan (outQueue x) cmd


-- | Source for all incoming commands from the XBee. This includes replies to framed command
-- that are also handled by sendCommand.
rawInSource :: XBeeM m => XBee -> BasicSource2 m CommandIn
rawInSource x = BasicSource2 first
    where first sink = execSTM (dupTChan (inQueue x)) >>= (flip step) sink
          step c (SinkCont next _) = execSTM (readTChan c) >>= next >>= step c
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
messagesSource :: XBeeM m => XBee -> BasicSource m ReceivedMessage
messagesSource x = rawInSource x $= commandInToReceivedMessage

-- | Source for modem status updates.
modemStatusSource :: XBeeM m => XBee -> BasicSource m ModemStatus
modemStatusSource x = rawInSource x $= commandInToModemStatus

commandInToModemStatus :: Transform CommandIn ModemStatus
commandInToModemStatus = T.filterMap step
    where step (ModemStatusUpdate s) = Just s
          step _ = Nothing
