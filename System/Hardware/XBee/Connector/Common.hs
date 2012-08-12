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
import qualified Data.SouSiT.Trans as T
import qualified Data.ByteString as BS
import Data.Serialize
import Control.Concurrent
import Control.Concurrent.ThreadGroup
import Control.Monad
import System.Hardware.XBee.Device
import System.Hardware.XBee.Command
import System.Hardware.XBee.Frame



-- | Sink that schedules the received. The sink is never done.
--   Pending timeouts are processed even after the sink is closed.
timeoutSink :: Sink Scheduled IO ()
timeoutSink = SinkCont next done
    where next i = (return i) >>= forkIO . schedule >> return (SinkCont next done)
          done = return ()
          schedule (Scheduled time action) = threadDelay time >> action


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
byteStringToCmdIn = (T.map BS.unpack) =$= T.disperse =$= bytesToCmdIn

-- | Parses CommandIn from bytes.
bytesToCmdIn = word8ToFrame =$= T.map frameToCommand =$= T.eitherRight

-- | Serializes CommandOut into ByteString.
cmdOutToByteString = T.map commandToFrame =$= T.map (runPut . put)

-- | Serializes CommandOut into bytes.
cmdOutToBytes = cmdOutToByteString =$= (T.map BS.unpack) =$= T.disperse


-- | Decorates a source/sink pair to also output everything received/sent to sysout.
--   The output is prefixed with either '>' or '<'
debugToSysout (src,sink) = (debugSource src, debugSink sink)

debugSource :: (Source src, Show a) => src IO a -> BasicSource IO a
debugSource = decorateSource (putStrLn . ("< "++) . show)

debugSink :: Show a => Sink a IO r -> Sink a IO r
debugSink   = decorateSink (putStrLn . ("> "++) . show)
