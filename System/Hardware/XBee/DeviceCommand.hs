module System.Hardware.XBee.DeviceCommand (
    execute,
    -- * Transmitting data
    transmitBytes,
    transmit,
    transmitNoAck,
    broadcast,
    -- * AT Settings
    atCommand,
    ATSetting,
    atSetting,
    getAT,
    setAT,
    -- * Addressing
    address16
) where

import Data.Word
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Time.Units
import Control.Monad
import System.Hardware.XBee.Device
import System.Hardware.XBee.Monad
import System.Hardware.XBee.Command


-- TODO really have that as a fixed value?
remoteTimeout = 10 :: Second
localTimeout = 1 :: Second

sendLocal  cmd handler = send localTimeout  (FrameCmd cmd handler)
sendRemote cmd handler = send remoteTimeout (FrameCmd cmd handler)



-- | Maximum number of bytes that can be transmitted with a single transmit call.
transmitBytes = 100

transmitCmd (XBeeAddress64 a) noack bdcst d f = Transmit64 f a noack bdcst (take transmitBytes d)
transmitCmd (XBeeAddress16 a) noack bdcst d f = Transmit16 f a noack bdcst (take transmitBytes d)

-- | Sends up to 100 bytes to another XBee and requests an ack.
transmit :: XBeeAddress -> [Word8] -> XBeeCmdAsync TransmitStatus
transmit to d = sendRemote cmd (liftM handle fetch)
    where handle (CRData (TransmitResponse _ r)) = r
          handle _ = TransmitNoAck
          cmd = transmitCmd to False False d

-- | Sends up to 100 bytes to another XBee without requesting an acknowledgement. There
--   is no way to tell whether the transmission succeeded or not.
transmitNoAck :: XBeeAddress -> [Word8] -> XBeeCmd ()
transmitNoAck to d = fire $ transmitCmd to True False d noFrameId

-- | Broadcasts up to 100 bytes to all XBees within the same network.
broadcast :: [Word8] -> XBeeCmd ()
broadcast d = fire $ transmitCmd (XBeeAddress64 broadcastAddress) True True d noFrameId


failOnLeft :: Monad m => Either String a -> m a
failOnLeft (Left err) = fail err
failOnLeft (Right v)  = return v


address16 :: ATSetting Address16
address16 = atSetting 'M' 'Y'


atCommand :: (Serialize i, Serialize o) => Char -> Char -> i -> XBeeCmdAsync o
atCommand c1 c2 i = sendLocal cmd (liftM handle fetch >>= failOnLeft)
    where cmd f = ATCommand f (commandName c1 c2) (ser i)
          handle (CRData (ATCommandResponse _ _ CmdOK d)) = parse d
          handle (CRData (ATCommandResponse _ _ status _)) = Left $ "Failed: " ++ show status
          handle _ = Left "Timeout"
          ser = BS.unpack . runPut . put
          parse = runGet get . BS.pack

data ATSetting a = ATSetting { getAT :: XBeeCmdAsync a,
                               setAT :: a -> XBeeCmdAsync () }

atSetting :: Serialize a => Char -> Char -> ATSetting a
atSetting c1 c2 = ATSetting (atCommand c1 c2 ()) (atCommand c1 c2)
