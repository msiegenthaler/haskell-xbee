module System.Hardware.XBee.DeviceCommand (
    -- * Transmitting data
    transmitBytes,
    transmit,
    transmitNoAck,
    broadcast
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

sendLocal  cmd handler = sendAsync localTimeout  (FrameCmd cmd handler)
sendRemote cmd handler = sendAsync remoteTimeout (FrameCmd cmd handler)



-- | Maximum number of bytes that can be transmitted with a single transmit call.
transmitBytes = 100

transmitCmd (XBeeAddress64 a) noack bdcst d f = Transmit64 f a noack bdcst (take transmitBytes d)
transmitCmd (XBeeAddress16 a) noack bdcst d f = Transmit16 f a noack bdcst (take transmitBytes d)

-- | Sends up to 100 bytes to another XBee and requests an ack.
transmit :: XBeeAddress -> [Word8] -> XBeeCmd (Future TransmitStatus)
transmit to d = sendAsync remoteTimeout $ FrameCmd cmd (liftM handle fetch)
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




address16 :: XBeeCmd (Future (Either String Address16))
address16 = sendLocal cmd (liftM handle fetch)
    where cmd f = ATCommand f (commandName 'M' 'Y') []
          handle (CRData (ATCommandResponse _ _ CmdOK d)) = runGet get (BS.pack d)
          handle (CRData (ATCommandResponse _ _ status _)) = Left $ "Failed: " ++ show status
          handle _ = Left "Timeout"
