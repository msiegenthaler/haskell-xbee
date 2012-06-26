module Main (
    main
) where

import Numeric
import Data.Word
import Data.Time.Units
import Data.Serialize
import Data.SouSiT
import Data.SouSiT.Handle
import Data.SouSiT.List
import qualified Data.SouSiT.Trans as T
import qualified Data.ByteString as BS
import System.Hardware.Serial
import System.Hardware.XBee.Frame
import System.Hardware.XBee.Device
import System.Hardware.XBee.DeviceCommand
import System.Hardware.XBee.Frame
import Control.Concurrent
import Control.Monad
import System.IO


portFile = "/dev/tty.usbserial-A6003ThW"
portSettings = defaultSettings { baudRate = B9600 }
fetchSize = 64
tmout = fromMicroseconds 2000000 :: Second

data1 :: [Word8]
data1 = [0x7e,0x00,0x04,0x08,0x01,0x4d,0x59,0x50]

main = withSerialPort portFile portSettings body
    where body h = let rawSrc  = serialPortSource h
                       src = rawSrc $= T.map (BS.unpack) =$= T.disperse
                       rawSink = serialPortSink h
                       sink = T.map BS.singleton =$ rawSink
                       src' = decorateSource (traceHex "in") src
                       sink' = decorateSink (traceHex "out") sink in
                execute' src' sink'
                -- TODO stop the XBee!


serialPortSource h = hSourceNoEOF rd h $= T.filter hasData
    where rd h = do r <- BS.hGetSome h fetchSize
                    unless (hasData r) $ threadDelay 100000
                    return r
          hasData d = BS.length d > 0

serialPortSink h = hSink BS.hPut h


execute src sink = do
        putStrLn "Instantiating XBee"
        xbee <- newDevice src sink
        putStrLn "Reading the Address16"
        let cmd = readAT address16 Local
        a16 <- sendCommandAndWaitIO xbee cmd tmout
        putStrLn $ "  " ++ (show a16)



execute' src sink = do
        putStrLn "Starting reader"
        let src' = src $= word8ToFrame
        reader <- forkIO (src' $$ sysoutSink "frame-in")
        putStrLn "Sending data"
        listSource data1 $$ sink
        putStrLn "Waiting for reply"
        threadDelay 1000000 -- 1s
        putStrLn "Stopping"
        killThread reader



devNullSink :: Monad m => Sink a m ()
devNullSink = SinkCont next (return ())
    where next i = return devNullSink


sysoutHexSink :: String -> Sink Word8 IO ()
sysoutHexSink l = SinkCont step done
    where step i = traceHex l i >> return (SinkCont step done)
          done = putStrLn $ l ++ " is now done"

traceHex pre i = putStrLn (pre ++ "> 0x" ++ showHex i "")

sysoutSink l = SinkCont step done
    where step i = putStrLn (l ++ "> " ++ show i) >> return (SinkCont step done)
          done = putStrLn $ l ++ " is now done"
