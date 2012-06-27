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


portFile = "/dev/tty.usbserial-A6003ThW"
portSettings = defaultSettings { baudRate = B9600 }
tmout = fromMicroseconds 2000000 :: Second


main = withSerialPort portFile portSettings body
    where body h = do (xbee,xif) <- newDevice
                      putStrLn "Starting XBee..."
                      stop <- connectToHandle xif h
                      putStrLn "XBee started."
                      execute xbee
                      putStrLn "Stopping XBee..."
                      stop
                      putStrLn "XBee stopped."

execute xbee = do
        putStrLn "Reading the Address16"
        a16 <- sendCommandAndWaitIO xbee (readAT address16 Local) tmout
        putStrLn $ "  => " ++ (show a16)
        putStrLn "Setting the Address16 to 0x1234"
        ok <- sendCommandAndWaitIO xbee (setAT address16 Local (Address16 0x1234)) tmout
        putStrLn $ "  => " ++ (show ok)
        putStrLn "Reading the Address16"
        na16 <- sendCommandAndWaitIO xbee (readAT address16 Local) tmout
        putStrLn $ "  => " ++ (show na16)
        putStrLn "Resetting the Address16"
        let (Right a16') = a16
        ok <- sendCommandAndWaitIO xbee (setAT address16 Local a16') tmout
        putStrLn $ "  => " ++ (show ok)
