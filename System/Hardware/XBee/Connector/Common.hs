module System.Hardware.XBee.Connector.Common (
    -- * Connector
    ThreadedConnector,
    sousiConnector,
    -- * Transformers
    byteStringToCmdIn,
    bytesToCmdIn,
    cmdOutToByteString,
    cmdOutToBytes,
    -- * Other
    timeoutSink,
    debugToSysout
) where

import Data.SouSiT
import Data.SouSiT.Sink
import Data.SouSiT.Transform
import qualified Data.SouSiT.Trans as T
import qualified Data.ByteString as BS
import Data.Word
import Data.Serialize
import Control.Monad
import Control.Concurrent
import Control.Concurrent.ThreadGroup
import System.Hardware.XBee.Device
import System.Hardware.XBee.Command
import System.Hardware.XBee.Frame



-- | Sink that schedules the received. The sink is never done.
--   Pending timeouts are processed even after the sink is closed.
timeoutSink :: Sink Scheduled IO ()
timeoutSink = actionSink $ void . forkIO . schedule
    where schedule (Scheduled time action) = threadDelay time >> action


type ThreadedConnector = XBeeConnector ThreadGroup

-- | XBeeConnector based upon a CommandIn source and a CommandOut sink.
sousiConnector :: Source src => IO (src IO CommandIn, Sink CommandOut IO ()) -> ThreadedConnector
sousiConnector c = XBeeConnector o close
    where o xif = c >>= open xif
          open xif (src,sink) = startThreadGroup [
                                    toSchedule xif $$ timeoutSink,
                                    src $$ incoming xif,
                                    outgoing xif $$ sink ]
          close = killThreadGroup

-- | Parses CommandIn from ByteStrings.
byteStringToCmdIn :: Transform BS.ByteString CommandIn
byteStringToCmdIn = T.map BS.unpack =$= T.disperse =$= bytesToCmdIn

-- | Parses CommandIn from bytes.
bytesToCmdIn :: Transform Word8 CommandIn
bytesToCmdIn = word8ToFrame =$= T.map frameToCommand =$= T.eitherRight

-- | Serializes CommandOut into ByteString.
cmdOutToByteString :: Transform CommandOut BS.ByteString
cmdOutToByteString = T.map commandToFrame =$= T.map encode

-- | Serializes CommandOut into bytes.
cmdOutToBytes :: Transform CommandOut Word8
cmdOutToBytes = cmdOutToByteString =$= T.map BS.unpack =$= T.disperse


-- | Decorates a source/sink pair to also output everything received/sent to sysout.
--   The output is prefixed with either '>' or '<'
debugToSysout :: (Show a, Show b, Source src) =>
        (src IO a, Sink b IO r) ->
        (SimpleSource IO a, Sink b IO r)
debugToSysout (src,sink) = (transformSource (T.debug "<") src, T.debug ">" =$ sink)
