module Main (
    main
) where

import Data.Time.Units
import Data.SouSiT
import Data.SouSiT.Trans as T
import System.Hardware.Serial
import System.Hardware.XBee.Command
import System.Hardware.XBee.Device
import System.Hardware.XBee.DeviceCommand
import System.Hardware.XBee.Connector.Common
import System.Hardware.XBee.Connector.Handle
import Control.Monad
import Control.Concurrent (threadDelay,forkIO)
import Control.Concurrent.STM


connector  f s = handleConnector $ openSerialPort f s
connector' f s = slowHandleConnector 30000 $ openSerialPort f s
connector1 = connector "/dev/tty.usbserial-A6003ThW" $ defaultSettings { baudRate = B9600 }
connector2 = connector' "/dev/tty.usbserial-A6003TfN" $ defaultSettings { baudRate = B9600 }

main = do runXBee connector1 cmd
          runXBee connector2 cmd
          return ()

cmd = do
        --fork outputRaws
        out "Address64 = " address64
        a16 <- getAT address16 >>= await
        output $ "Address16 = " ++ show a16
        setAT address16 (Address16 0x1234) >>= await
        out "New Address16 = " $ getAT address16
        out "NodeIdentifier = " $ getAT nodeIdentifier
        out "PAN-ID = " $ getAT panId
        out "Hardware Version = " hardwareVersion
        setAT address16 a16 >>= await
        nodes <- discover (100 :: Millisecond) >>= await
        mapM_ (output . ("   " ++) . formatNode) nodes
        --fork $ out "Address64 from other Thread" address64
        output "Done"


formatNode n = "Node " ++ show (nodeAddress64 n) ++ " with " ++ show (nodeSignalStrength n)

outputMsgs = messagesSource $$ T.map show =$ liftIOSink outSink

outputRaws = rawInSource $$ T.map show =$ liftIOSink outSink

liftIOSink :: Sink a IO r -> Sink a XBeeCmd r
liftIOSink (SinkCont n d) = SinkCont n' (liftIO d)
    where n' a = liftIO (liftM liftIOSink (n a))
liftIOSink (SinkDone d) = SinkDone $ liftIO d

outSink :: Sink String IO ()
outSink = SinkCont o (return ())
    where o s = putStrLn s >> return outSink

output = liftIO . putStrLn
out pre c = c >>= await >>= o
    where o i = output $ pre ++ show i
