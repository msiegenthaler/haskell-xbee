module Main (
    main
) where

import Data.Time.Units
import System.Hardware.Serial
import System.Hardware.XBee.Command
import System.Hardware.XBee.Device
import System.Hardware.XBee.DeviceCommand
import System.Hardware.XBee.Connector.Handle
import Control.Monad
import Control.Concurrent.STM


portFile = "/dev/tty.usbserial-A6003ThW"
portSettings = defaultSettings { baudRate = B9600 }
tmout = fromMicroseconds 2000000 :: Second


main = withSerialPort portFile portSettings body
    where body h = do (xbee,xif) <- atomically $ newDevice
                      putStrLn "Starting XBee..."
                      connector <- connectToHandle xif h
                      putStrLn "XBee started."
                      exec xbee
                      putStrLn "Stopping XBee..."
                      stopConnector connector
                      putStrLn "XBee stopped."

exec :: XBee -> IO ()
exec xbee = do
        a64 <- execute xbee address64
        putStrLn $ "Address64 = " ++ (show a64)
        a16 <- execute xbee $ getAT address16
        putStrLn $ "Address16 = " ++ (show a16)
        execute xbee $ setAT address16 (Address16 0x1234)
        execute xbee (getAT address16) >>= putStrLn . ("New Address16 = " ++) . show
        nid <- execute xbee $ getAT nodeIdentifier
        putStrLn $ "NodeIdentifier = " ++ show nid
        execute xbee $ setAT nodeIdentifier "Mario's device"
        pid <- execute xbee $ getAT panId
        putStrLn $ "PAN-ID = " ++ show pid
        hv <- execute xbee hardwareVersion
        putStrLn $ "Hardware Version = " ++ show hv
        -- Reset
        execute xbee $ setAT address16 a16
        execute xbee softwareReset
