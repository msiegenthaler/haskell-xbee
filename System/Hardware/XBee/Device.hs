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
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Identity
import Data.SouSiT
import qualified Data.SouSiT.Trans as T


data CommandResult = CRData CommandOut
                   | CRPurged
                   | CRTimeout deriving (Show, Eq)
type CommandResultChan = TChan CommandResult

type CommandHandler a = Sink CommandIn Identity a
data CommandSpec r = CommandSpec (FrameId -> CommandOut) (CommandHandler r)

type ResponseMap = Map FrameId CommandResultChan

-- | Opaque representation of the locally attached XBee device.
data XBee = XBee { inQueue  :: TChan CommandIn,
                   outQueue :: TChan CommandOut,
                   currentFrame :: TVar FrameId,
                   pending :: TVar ResponseMap,
                   threads  :: (ThreadId, ThreadId) }




-- | Creates a new XBee device based upon a byte source and sink.
newDevice :: Source src => src IO Word8 -> Sink Word8 IO () -> IO XBee
newDevice src sink = do
        inQ  <- newTChanIO
        outQ <- newTChanIO
        f <- newTVarIO frameId
        p <- newTVarIO Map.empty
        r <- forkReader src' inQ
        w <- forkWriter sink' outQ
        return $ XBee inQ outQ f p (r,w)
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


enqueue :: XBee -> (FrameId -> CommandOut) -> STM CommandResultChan
enqueue x cmd = do
        f   <- reserveFrame x
        r   <- newTChan
        pm  <- readTVar (pending x)
        pm' <- setPending pm f r
        writeTVar (pending x) pm'
        writeTChan (outQueue x) (cmd f)
        return r

reserveFrame :: XBee -> STM FrameId
reserveFrame x = let frame = currentFrame x in do
        f <- readTVar frame
        writeTVar frame (nextFrame f)
        return f

setPending :: ResponseMap -> FrameId -> CommandResultChan -> STM ResponseMap
setPending pm frame h = expire (Map.lookup frame pm) >> return (Map.insert frame h pm)
        where expire (Just c) = writeTChan c CRPurged >> return ()
              expire Nothing   = return ()
