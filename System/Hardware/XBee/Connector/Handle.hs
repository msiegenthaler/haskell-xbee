module System.Hardware.XBee.Connector.Handle (
    handleConnector
) where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.ThreadGroup
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


type HandleConnector = XBeeConnector ThreadGroup

-- | XBee connector that uses a ReadWrite handle as the backing interface. EOFs are ignored.
handleConnector :: IO Handle -> HandleConnector
handleConnector h = XBeeConnector o close
    where o xif = h >>= open xif

open xif h = startThreadGroup [ toSchedule xif $$ timeoutSink,
                                src $$ incoming xif,
                                outgoing xif $$ sink ]
    where src = commandSource h
          sink = commandSink h

close = killThreadGroup



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
