module Main (
    main
) where

import Data.Word
import Data.Time.Units
import Data.SouSiT
import Data.SouSiT.Sink
import qualified Data.SouSiT.Trans as T
import qualified Codec.Binary.UTF8.String as S
import System.Hardware.Serial
import System.Hardware.XBee.Command
import System.Hardware.XBee.Device
import System.Hardware.XBee.DeviceCommand
import System.Hardware.XBee.Connector.Handle
import Control.Monad.IO.Class as I
import Control.Concurrent.ThreadGroup
import Print


connector  f s = handleConnector $ openSerialPort f s
connector' f s = slowHandleConnector 30000 $ openSerialPort f s
connector1 = connector  "/dev/tty.usbserial-A6003ThW" $ defaultSettings { baudRate = B9600 }
connector2 = connector' "/dev/tty.usbserial-A6003TfN" $ defaultSettings { baudRate = B9600 }


main = withSysoutPrint $ startThreadGroup [
                runXBee connector1 $ initXBee "One" >> sayHi >> showPeers >> sayHiToAll >> sayBye,
                runXBee connector2 $ initXBee "Two" >> sayHi >> echoMsgs
            ] >>= awaitThreadGroup >> printLn "Done."
          

initXBee n = do a  <- setAT address16 disabledAddress
                ni <- setAT nodeIdentifier n
                await (a >> ni)

sayHi = do a <- address64 >>= await
           i <- getAT nodeIdentifier >>= await
           output $ "XBee connected: " ++ show a ++ " (" ++ show i ++ ")"

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
    where formatNode n = "- " ++ show (nodeAddress64 n) ++ " with " ++
                show (nodeSignalStrength n)

sayHiToAll = do
        broadcast $ S.encode "Hi everybody"
        nodes <- discover (300 :: Millisecond) >>= await
        output $ "Saying hi to all " ++ show (length nodes) ++ " peers"
        c <- mapM sendHi nodes
        mapM_ await c
    where sendHi n = transmit to $ S.encode $ "Hi, you're " ++ show (nodeAddress64 n)
            where to = XBeeAddress64 $ nodeAddress64 n


sayBye = broadcast endSignal

receiveMsgs = output "Waiting for messages.." >> outputMsgs

outputMsgs = messagesSource $$ T.map format =$ outSink
    where format msg = "Got '" ++ S.decode (messageBody msg) ++ "' from " ++ showSender (sender msg)
          showSender (XBeeAddress16 a) = show a
          showSender (XBeeAddress64 a) = show a


outputRaws = rawInSource $$ T.map show =$ outSink


endSignal = [0]

echoMsgs = messagesSource $$ T.mapM deco =$ replySink (fun . messageBody)
    where fun body | body == endSignal = Nothing
                   | otherwise         = Just $ S.encode "You said " ++ body
          deco i = (output . ("> echoing to: " ++) . S.decode . messageBody) i >> return i


replySink :: (ReceivedMessage -> Maybe [Word8]) -> Sink ReceivedMessage XBeeCmd ()
replySink f = do msg <- inputMaybe
                 let r = msg >>= f
                 maybe (return ()) ((>> replySink f) . return . rpl msg) r
    where rpl (Just msg) answer = transmit (sender msg) answer >>= await >> return ()
          rpl _ _ = return ()


outSink :: MonadIO m => Sink String m ()
outSink = actionSink (I.liftIO . printLn)


output = liftIO . printLn

out pre c = c >>= await >>= o
    where o i = output $ pre ++ show i
