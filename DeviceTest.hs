module Main (
    main
) where

import Data.Word
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Time.Units
import Data.SouSiT
import Data.SouSiT.Trans as T
import qualified Codec.Binary.UTF8.String as S
import System.IO
import System.Hardware.Serial
import System.Hardware.XBee.Command
import System.Hardware.XBee.Device
import System.Hardware.XBee.DeviceCommand
import System.Hardware.XBee.Connector.Common
import System.Hardware.XBee.Connector.Handle
import Control.Monad
import Control.Concurrent (threadDelay,forkIO)
import Control.Concurrent.STM
import Control.Concurrent.ThreadGroup
import Print


connector  f s = handleConnector $ openSerialPort f s
connector' f s = slowHandleConnector 30000 $ openSerialPort f s
connector1 = connector  "/dev/tty.usbserial-A6003ThW" $ defaultSettings { baudRate = B9600 }
connector2 = connector' "/dev/tty.usbserial-A6003TfN" $ defaultSettings { baudRate = B9600 }


main = withSysoutPrint $ startThreadGroup [
                runXBee connector1 $ sayHi >> showPeers >> sayHiToAll,
                runXBee connector2 $ sayHi >> receiveMsgs
            ] >>= awaitThreadGroup >> printLn "Done."
          

sayHi = out "XBee connected: " address64

diagnostics = do
        out "Address64 = " address64
        a16 <- getAT address16 >>= await
        output $ "Address16 = " ++ show a16
        setAT address16 (Address16 0x1234) >>= await
        out "New Address16 = " $ getAT address16
        out "NodeIdentifier = " $ getAT nodeIdentifier
        out "PAN-ID = " $ getAT panId
        out "Hardware Version = " hardwareVersion
        setAT address16 a16 >>= await
        showPeers

showPeers = do
        nodes <- discover (300 :: Millisecond) >>= await
        output $ "Found " ++ show (length nodes) ++ " peers:"
        mapM_ (output . ("   " ++) . formatNode) nodes
    where formatNode n = "Node " ++ show (nodeAddress64 n) ++ " with " ++
                show (nodeSignalStrength n)

sayHiToAll = do
        broadcast $ S.encode "Hi everybody"
        nodes <- discover (300 :: Millisecond) >>= await
        output $ "Saying hi to all " ++ show (length nodes) ++ " peers"
        mapM_ sendHi nodes
    where sendHi n = transmit to $ S.encode $ "Hi, you're " ++ show (nodeAddress64 n)
            where to = XBeeAddress64 $ nodeAddress64 n



receiveMsgs = output "Waiting for messages.." >> outputMsgs

outputMsgs = messagesSource $$ T.map format =$ liftIOSink outSink
    where format msg = "Got '" ++ S.decode (messageBody msg) ++ "' from " ++ showSender (sender msg)
          showSender (XBeeAddress16 a) = show a
          showSender (XBeeAddress64 a) = show a


outputRaws = rawInSource $$ T.map show =$ liftIOSink outSink


liftIOSink :: Sink a IO r -> Sink a XBeeCmd r
liftIOSink (SinkCont n d) = SinkCont n' (liftIO d)
    where n' a = liftIO (liftM liftIOSink (n a))
liftIOSink (SinkDone d) = SinkDone $ liftIO d


outSink :: Sink String IO ()
outSink = SinkCont o (return ())
    where o s = printLn s >> return outSink


output = liftIO . printLn

out pre c = c >>= await >>= o
    where o i = output $ pre ++ show i
