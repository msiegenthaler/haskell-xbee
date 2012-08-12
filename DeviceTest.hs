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


main = runXBee con cmd
    where con = handleConnector $ openSerialPort portFile portSettings

cmd = do
        out "Address64 = " address64
        a16 <- getAT address16 >>= await
        output $ "Address16 = " ++ show a16
        setAT address16 (Address16 0x1234) >>= await
        out "New Address16 = " $ getAT address16
        out "NodeIdentifier = " $ getAT nodeIdentifier
        out "PAN-ID = " $ getAT panId
        out "Hardware Version = " hardwareVersion
        setAT address16 a16 >>= await
        nodes <- discover (300 :: Millisecond) >>= await
        mapM (output . ("   " ++) . formatNode) nodes
        output "Done"
    where formatNode n = "Node " ++ (show $ nodeAddress64 n) ++ " with " ++
                             (show $ nodeSignalStrength n)

output = liftIO . putStrLn
out pre c = c >>= await >>= o
    where o i = output (pre ++ (show i))
