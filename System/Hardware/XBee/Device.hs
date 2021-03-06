{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Hardware.XBee.Device (
    -- * Device
    runXBee,
    -- * Device Interface
    XBeeConnector(..),
    XBeeInterface(..),
    Scheduled(..),
    -- * Actions
    XBeeCmd,
    XBeeCmdAsync,
    fork,
    localTimeout,
    remoteTimeout,
    setTimeouts,
    -- ** Fire (without response)
    fire,
    -- ** Send (with response)
    send,
    CommandHandler,
    CommandResponse(..),
    FrameCmd(..),
    -- ** Source
    rawInSource,
    ReceivedMessage(..),
    messagesSource,
    modemStatusSource,
    -- ** Future
    Future,
    await,
    awaitAny,
    afterUs,
    instantly,
    fasterOf,
    fastestOf,
    -- * Reexports
    liftIO
) where

import System.Hardware.XBee.Command
import Data.ByteString (ByteString)
import Data.Time.Units
import qualified Control.Concurrent as C
import Control.Concurrent.STM
import Control.Concurrent.Future hiding (await,awaitAny,afterUs)
import qualified Control.Concurrent.Future as F
import Control.Exception as E (bracket,bracket_)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Control.RequestResponseCorrelator
import Data.SouSiT
import Data.SouSiT.Sink
import Data.SouSiT.Source
import Data.SouSiT.STM
import qualified Data.SouSiT.Trans as T


-- | Command monad for a XBees.
newtype XBeeCmd a = XBeeCmd { runXBeeCmd :: StateT Timeouts (ReaderT XBee IO) a }
    deriving (Monad, MonadIO, MonadReader XBee, MonadState Timeouts, Functor, Applicative)

-- | Command monad with a future as the result.
type XBeeCmdAsync a = XBeeCmd (Future a)




-- | Answers received to a command sent to the XBee.
data CommandResponse = CRData CommandIn
                     | CRPurged
                     | CRTimeout deriving (Show, Eq)

-- | Handler for the answers to a single command sent to the XBee.
type CommandHandler a = Fetch CommandResponse a

-- | A command that expects an answer from the XBee.
data FrameCmd a   = FrameCmd (FrameId -> CommandOut) (CommandHandler a)
instance Functor FrameCmd where
    fmap f (FrameCmd a handler) = FrameCmd a (fmap f handler)



-- | Task to be scheduled. Delay in microseconds and the action to execute after the delay.
data Scheduled = Scheduled Int (IO ())

data Timeouts = Timeouts { tmoutLocal :: Microsecond,
                           tmoutRemote :: Microsecond }
defaultTimeouts = Timeouts (convertUnit (1500 :: Millisecond)) (convertUnit (6 :: Second))

type XBeeCorrelator =  Correlator FrameId CommandResponse




-- | Opaque representation of the locally attached XBee device.
data XBee = XBee { scheduleQueue :: TChan Scheduled,
                   outQueue  :: TChan CommandOut,
                   inQueue   :: TChan CommandIn,
                   correlator :: XBeeCorrelator,
                   users :: TVar Int }

-- | Interface to an XBee. This is 'side' of the xbee that needs to be attached to the
--   actual device. See i.e. the HandleDeviceConnector module.
data XBeeInterface = XBeeInterface {
                    -- | Commands that are sent to the xbee device (serial port).
                    outgoing   :: FeedSource IO CommandOut,
                    -- | Commands received from the xbee device (serial port).
                    incoming   :: Sink CommandIn IO (),
                    -- | Actions to be scheduled with the specified delay.
                    --   This is used mainly for timeouts.
                    toSchedule :: FeedSource IO Scheduled }

-- | Connects an XBee device to a "backend", i.e. a serial port.
data XBeeConnector a = XBeeConnector {
        openConnector :: XBeeInterface -> IO a,
        closeConnector :: a -> IO () }


-- | Runs an XBee command with the specified connector.
runXBee :: XBeeConnector x -> XBeeCmd a -> IO a
runXBee connector cmd = do
        (xbee, xif) <- atomically newDevice
        bracket (openConnector connector xif) (closeConnector connector) (run xbee)
    where run x _ = execute x cmd <* atomically (awaitZero (users x))

awaitZero :: TVar Int -> STM ()
awaitZero v = readTVar v >>= w
    where w 0 = return ()
          w _ = retry

execute :: XBee -> XBeeCmd a -> IO a
execute x cmd = liftM fst $ bracket_ allocate deallocate run
    where run = runReaderT (runStateT (runXBeeCmd cmd) defaultTimeouts) x
          allocate = atomically $ modifyTVar (users x) (+ 1)
          deallocate = atomically $ modifyTVar (users x) (subtract 1)

--- | Create a new XBee device along with the corresponding interface.
newDevice :: STM (XBee, XBeeInterface)
newDevice = do
        inQ  <- newTChan
        outQ <- newTChan
        scdQ <- newTChan
        corr <- newCorrelator CRPurged
        uc   <- newTVar 0
        let xbee = XBee scdQ outQ inQ corr uc
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


-- | Forks a new thread that executes XBeeCmd
fork :: XBeeCmd a -> XBeeCmd C.ThreadId
fork cmd = ask >>= liftIO . C.forkIO . flip execute (void cmd)

-- IO lifted functions from Future
await = liftIO' . F.await
awaitAny = liftIO' . F.awaitAny
afterUs a us = liftIO' $ F.afterUs a us
liftIO' :: IO a -> XBeeCmd a
liftIO' = liftIO

-- | Sends a command without waiting for a response.
-- Use noFrameId if the command supports frames.
fire :: CommandOut -> XBeeCmd ()
fire cmd = liftM outQueue ask >>= liftIO . atomically . flip writeTChan cmd

-- | Sends a command.
send :: TimeUnit time => time -> FrameCmd a -> XBeeCmdAsync a
send tmo cmd = do
        x   <- ask
        fut <- liftIO $ atomically $ sendCommand x tmo cmd
        return $ stmFuture fut

sendCommand :: TimeUnit time => XBee -> time -> FrameCmd a -> STM (STM a)
sendCommand x tmo (FrameCmd cmd h) = do
        (frame, future, feedFun) <- request (correlator x) h
        writeTChan (outQueue x) (cmd frame)
        writeTChan (scheduleQueue x) $ Scheduled tmoUs (atomically $ feedFun CRTimeout)
        return future
    where tmoUs = fromIntegral $ toMicroseconds tmo

-- | Timeout for XBee local commands (ATs and such).
localTimeout :: XBeeCmd Microsecond
localTimeout = liftM tmoutLocal get

-- | Timeout for commands that involve other XBees.
remoteTimeout :: XBeeCmd Microsecond
remoteTimeout = liftM tmoutRemote get

-- | Set the timeout for commands.
setTimeouts :: (TimeUnit local, TimeUnit remote) => local -> remote -> XBeeCmd ()
setTimeouts lcal remote = put $ Timeouts (convertUnit lcal) (convertUnit remote)


-- | An incoming message (abstracts over Receive64 and Receive16)
data ReceivedMessage = ReceivedMessage { sender :: XBeeAddress,
                                         signal :: SignalStrength,
                                         addressBroadcast :: Bool,
                                         panBroadcast :: Bool,
                                         messageBody :: ByteString } deriving (Show,Eq)

-- | Source for all incoming commands from the XBee. This includes replies to framed command
-- that are also handled by a CommandHandler from send.
rawInSource :: FeedSource XBeeCmd CommandIn
rawInSource = FeedSource fun
    where fun sink = open >>= trans sink
          open = liftM inQueue ask >>= liftIO . atomically . dupTChan
          trans sink chan = sinkStatus sink >>= handle
            where handle (Done _)    = return sink
                  handle (Cont nf _) = dequeue chan >>= nf

dequeue :: TChan a -> XBeeCmd a
dequeue = liftIO . atomically . readTChan

-- Transformer for CommandIn into ReceivedMessage.
commandInToReceivedMessage :: Transform CommandIn ReceivedMessage
commandInToReceivedMessage = T.filterMap step
    where step (Receive16 se si ab pb d) = Just $ ReceivedMessage (XBeeAddress16 se) si ab pb d
          step (Receive64 se si ab pb d) = Just $ ReceivedMessage (XBeeAddress64 se) si ab pb d
          step _ = Nothing

-- | Source for all messages received from remote XBees (Receive16 and Receive64).
messagesSource :: SimpleSource XBeeCmd ReceivedMessage
messagesSource = rawInSource $= commandInToReceivedMessage

-- | Source for modem status updates.
modemStatusSource ::  SimpleSource XBeeCmd ModemStatus
modemStatusSource = rawInSource $= commandInToModemStatus

commandInToModemStatus :: Transform CommandIn ModemStatus
commandInToModemStatus = T.filterMap step
    where step (ModemStatusUpdate s) = Just s
          step _ = Nothing
