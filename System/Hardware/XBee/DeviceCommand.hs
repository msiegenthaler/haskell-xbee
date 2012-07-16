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
    address16,
    address64,
    nodeIdentifierMaxLength,
    nodeIdentifier
) where

import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Time.Units
import qualified Codec.Binary.UTF8.String as S
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


-- | 16-bit network address of the xbee.
address16 :: ATSetting Address16
address16 = atSetting 'M' 'Y'

-- | Immutable 64-bit network address of the xbee.
address64 :: XBeeCmdAsync Address64
address64 = do
        lf <- getAT $ a64p 'L'
        hf <- getAT $ a64p 'H'
        l <- await lf
        h <- await hf
        instantly $ Address64 $ (fromIntegral l) .|. (shift (fromIntegral h) 32)
    where
        a64p :: Char -> ATSetting Word32
        a64p = atSetting 'S'


newtype RawData = RawData BS.ByteString
instance Serialize RawData where
    get = remaining >>= getByteString >>= return . RawData
    put (RawData bs) = putByteString bs

newtype Utf8String = Utf8String { unpackUtf8 :: String }
instance Serialize Utf8String where
    get = remaining >>= getByteString >>= return . Utf8String . S.decode . BS.unpack
    put = putByteString . BS.pack . S.encode . unpackUtf8

-- | Shortens the string (removes at the end) to make sure that it can be encoded in utf-8
--   in at most n characters.
takeBytes :: Int -> String -> String
takeBytes n = process . take n
    where process [] = []
          process i = let e = S.encode i in
                if (length e) > n then process $ take ((length i) - 1) i
                else i

mapAtSetting :: ATSetting a -> (a -> b) -> (b -> a) -> ATSetting b
mapAtSetting (ATSetting gf sf) f1 f2 = ATSetting gf' (sf . f2)
    where gf' = liftM (fmap f1) gf


nodeIdentifierMaxLength = 20

-- | String to identify a node.
nodeIdentifier :: ATSetting String
nodeIdentifier = mapAtSetting (atSetting 'N' 'I') unpackUtf8 (Utf8String . takeBytes 100)