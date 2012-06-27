module System.Hardware.XBee.Connector.Handle (
    connectToHandle
) where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Data.SouSiT
import Data.SouSiT.Handle
import qualified Data.SouSiT.Trans as T
import qualified Data.ByteString as BS
import Data.Serialize
import System.IO
import System.Hardware.XBee.Device
import System.Hardware.XBee.Command
import System.Hardware.XBee.Frame
import System.Hardware.XBee.Connector.Common


type StopAction = IO ()

-- | Connects the XBeeInterface to a handle. The handle must be open in ReadWrite. EOFs
--   are ignored.
--   The scheduler is started (using Common.timeoutSink)
connectToHandle :: XBeeInterface -> Handle -> IO StopAction
connectToHandle xif h = do
        st <- forkIO (toSchedule xif $$ timeoutSink)
        rt <- forkIO (src $$ incoming xif)
        wt <- forkIO (outgoing xif $$ sink)
        return $ (mapM killThread [st,rt,wt] >> return ())
    where src = commandSource h
          sink = commandSink h



fetchSize = 256

emptyInputDelay = 10000 -- in microseconds

commandSink h = T.map commandToFrame =$= T.map (runPut . put) =$ hSink BS.hPut h

commandSource h = (streamByteSource 256 h) $= (T.map BS.unpack) =$= T.disperse =$=
        word8ToFrame =$= T.map frameToCommand =$= T.eitherRight

streamByteSource fetchSize h = hSourceNoEOF rd h $= T.filter hasData
    where rd h = do r <- BS.hGetSome h fetchSize
                    unless (hasData r) $ threadDelay emptyInputDelay
                    return r
          hasData d = BS.length d > 0
