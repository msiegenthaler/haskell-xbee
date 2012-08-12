module System.Hardware.XBee.DeviceCommand (
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
    nodeIdentifier,
    panId,
    -- * Discover
    NodeInformation(..),
    discover,
    -- * Various
    hardwareVersion,
    softwareReset
) where

import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Time.Units
import qualified Codec.Binary.UTF8.String as S
import Control.Monad
import Control.Applicative
import System.Hardware.XBee.Device
import System.Hardware.XBee.Command


-- TODO really have that as a fixed value?
remoteTimeout = 10 :: Second
localTimeout = 2 :: Second

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
address64 = combine <$> getAT (a64p 'L') <*> getAT (a64p 'H')
    where
        a64p :: Char -> ATSetting Word32
        a64p = atSetting 'S'
        combine lf hf = do
            l <- lf
            h <- hf
            return $ Address64 $ (fromIntegral l) .|. (shift (fromIntegral h) 32)


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
nodeIdentifier = mapAtSetting (atSetting 'N' 'I') unpackUtf8 enc
    where enc = Utf8String . takeBytes nodeIdentifierMaxLength

-- | Used to set and read the PAN (Personal Area Network) ID of the xbee.
--   Only modules with matching PAN IDs can communicate with each other
panId :: ATSetting Word16
panId = atSetting 'I' 'D'

-- | Hardware version of the xbee.
hardwareVersion :: XBeeCmdAsync Word16
hardwareVersion = atCommand 'H' 'V' ()

-- | Used to force a software reset on the RF module. The reset simutates powering off
-- and then on again the xbee module.
softwareReset :: XBeeCmdAsync ()
softwareReset = atCommand 'F' 'R' ()


data NodeInformation = NodeInformation {
    nodeAddress16 :: Address16,
    nodeAddress64 :: Address64,
    nodeSignalStrength :: SignalStrength,
    nodeId :: String } deriving (Show,Eq)
instance Serialize NodeInformation where
    get = NodeInformation <$> get <*> get <*> get <*>
        (get >>= return . takeWhile (/= '\0' ) . unpackUtf8)
    put (NodeInformation a16 a64 sst nid) = put a16 >> put a64 >> put sst >> put (Utf8String nid)


-- | Discovers all nodes on that can be reached from this xbee. Does not include this
--   xbee.
--   All modules on the current operating channel and PAN ID are found.
discover :: TimeUnit time => time -> XBeeCmdAsync [NodeInformation]
discover tmo = setAT discoverTimeout (convertUnit tmo) >>= await >>
               setAT discoverSelfResponse False >>= await >>
               discover' tmo'
    where tmo' = convertUnit tmo + localTimeout

discover' :: TimeUnit time => time -> XBeeCmdAsync [NodeInformation]
discover' tmout = send tmout $ FrameCmd cmd (fetch >>= handle [])
    where cmd f = ATCommand f (commandName 'N' 'D') []
          handle :: [NodeInformation] -> CommandResponse -> CommandHandler [NodeInformation]
          handle soFar (CRData (ATCommandResponse _ _ CmdOK [])) = return soFar
          handle soFar (CRData (ATCommandResponse _ _ CmdOK d)) = do ni <- parse d
                                                                     fetch >>= handle (ni:soFar)
          handle soFar (CRData (ATCommandResponse _ _ status _)) = fail $ "Failed: " ++ show status
          handle _ _ = fail "Timeout"
          parse = failOnLeft . runGet get . BS.pack

-- | Set/Get the node discover timeout (only used internally).
discoverTimeout :: ATSetting Millisecond
discoverTimeout = mapAtSetting (atSetting 'N' 'T') convertToMs (max 1 . min 252 . convertFromMs)
    where nt :: ATSetting Word8
          nt = atSetting 'N' 'T'
          convertToMs :: Word8 -> Millisecond
          convertToMs b = fromIntegral (b * 100)
          convertFromMs :: Millisecond -> Word8
          convertFromMs v = round $ (fromIntegral $ toMicroseconds v) / 100000

-- | Controls if a node discover dows return the sender as well (only used internally).
discoverSelfResponse :: ATSetting Bool
discoverSelfResponse = atSetting 'N' 'O'
