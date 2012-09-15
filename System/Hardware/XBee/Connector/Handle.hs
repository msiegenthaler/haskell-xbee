module System.Hardware.XBee.Connector.Handle (
    -- * Connector
    handleConnector,
    slowHandleConnector,
    handleDebugConnector,
    -- * Low-Level
    portSource,
    portSink
) where

import Data.SouSiT
import Data.SouSiT.Handle
import qualified Data.SouSiT.Trans as T
import qualified Data.ByteString as BS
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Applicative
import System.Hardware.XBee.Connector.Common


fetchSize = 256
emptyInputDelay = 10000 -- in microseconds


-- | XBee connector that uses a ReadWrite handle as the backing interface. EOFs are ignored.
handleConnector :: IO Handle -> ThreadedConnector
handleConnector handle = sousiConnector (mk <$> handle)
    where mk h = (portCommandSource h, portCommandSink h)

-- | In some cases the XBee (or the serial port) is too slow which results in commands running
--   into timeouts because the request have been droppen.
slowHandleConnector slowUs handle = sousiConnector (mk <$> handle)
    where mk h = (portCommandSource h, T.mapM slowdown $ portCommandSink h)
          slowdown i = threadDelay slowUs >> return i

-- | Same as handleConnector, but writes all in and out commands to the system out.
handleDebugConnector :: IO Handle -> ThreadedConnector
handleDebugConnector handle = sousiConnector (mk <$> handle)
    where mk h = debugToSysout (portCommandSource h, portCommandSink h)


portSink = hSink BS.hPut

portCommandSink h = cmdOutToByteString =$ portSink h

portSource :: Int -> Handle -> SimpleSource IO BS.ByteString
portSource size handle = hSourceNoEOF rd handle $= T.filter hasData
    where rd h = do r <- BS.hGetSome h size
                    unless (hasData r) $ threadDelay emptyInputDelay
                    return r
          hasData d = BS.length d > 0

portCommandSource h = portSource fetchSize h $= byteStringToCmdIn
