module System.Hardware.XBee.Command (
    -- * FrameId
    FrameId,
    noFrameId,
    frameId,
    nextFrame,
    -- * Address
    Address64(..),
    Address16(..),
    broadcastAddress,
    disabledAddress,
    -- * Command
    CommandName,
    commandName,
    CommandStatus(..),
    ModemStatus(..),
    DisableAck,
    BroadcastPan,
    SignalStrength,
    dBm,
    fromDbm,
    TransmitStatus(..),
    CommandIn(..),
    CommandOut(..)
) where

import Data.Word
import Data.Bits
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Monad
import Control.Applicative


newtype FrameId = FrameId Word8 deriving (Show, Eq)
-- | Don't use a FrameId.
noFrameId = FrameId 0
-- | Initial FrameId.
frameId = FrameId 1
-- | The next FrameId. The ids are looped (after FrameId 255 follows FrameId 1)
nextFrame (FrameId 255) = frameId --overflow
nextFrame (FrameId i)   = FrameId (i+1)
instance Serialize FrameId where
    get = liftM FrameId getWord8
    put (FrameId i) = putWord8 i


-- | Address of an XBee device.
newtype Address64 = Address64 Word64 deriving (Show, Eq)
-- | Address for broadcasts to all XBee devices.
broadcastAddress = Address64 0xFFFF
instance Serialize Address64 where
    get = liftM Address64 getWord64be
    put (Address64 a) = putWord64be a

-- | 16-bit network address of an XBee device.
newtype Address16 = Address16 Word16 deriving (Show, Eq)
-- | Address to disable 16-bit addressing.
disabledAddress  = Address16 0xFFFE
instance Serialize Address16 where
    get = liftM Address16 getWord16be
    put (Address16 a) = putWord16be a



data ModemStatus = HardwareReset
                 | WatchdogTimerReset
                 | Associated
                 | Disassociated
                 | SyncLost
                 | CoordinatorRealignment
                 | CoordinatorStarted deriving (Enum, Show, Eq, Bounded)

newtype CommandName = CommandName (Word8, Word8) deriving (Eq)
instance Show CommandName where
    show (CommandName (c1, c2)) = "Command " ++ [toChar c1,toChar c2]
        where toChar = toEnum . fromIntegral
commandName :: Char -> Char -> CommandName
commandName a b = CommandName (c2w a, c2w b)
    where c2w = fromIntegral . fromEnum

data CommandStatus = CmdOK
                   | CmdError
                   | CmdInvalidCommand
                   | CmdInvalidParameter deriving (Enum, Show, Eq, Bounded)

-- | Disable acknowledgment.
type DisableAck = Bool
-- | Send packet with broadcast Pan ID.
type BroadcastPan = Bool
type AddressBroadcast = Bool
type PanBroadcast = Bool

-- | Status/Result of a transmitted packet.
data TransmitStatus =
                      -- | Packet delivered to the remote XBee.
                      TransmitSuccess
                      -- | All retries are expired and no ACK is received.
                      -- Does not apply to broadcasts.
                    | TransmitNoAck
                      -- | Clear Channel Assessment failure. The channel was not available for
                      -- transmission within the retries (normally two retries).
                    | TransmitCcaFailure
                      -- |  Coordinator times out of an indirect transmission.
                      -- Timeout is defined as (2.5 x SP (Cyclic Sleep Period) parameter value).
                      -- Does not apply to broadcasts.
                    | TransmitPurged deriving (Enum, Show, Eq, Bounded)


newtype SignalStrength = SignalStrength Word8 deriving (Eq)
-- | Signal strength in dBm (negative value, 0 is best).
dBm :: SignalStrength -> Int
dBm (SignalStrength s) = 0 - (fromIntegral s)
-- | Creates a signal strength. Values >0 and <255 will be truncated to 0 resp. 255.
fromDbm :: Int -> SignalStrength
fromDbm v | v > 0      = SignalStrength 0
          | v < (-255) = SignalStrength 255
          | otherwise  = SignalStrength (fromIntegral (-v))
instance Show SignalStrength where
    show = (++ " dBm") . show . dBm
instance Ord SignalStrength where
    a <= b = (dBm a) <= (dBm b)

-- | Commands or responses sent from the XBee to the computer.
data CommandIn =  ModemStatusUpdate ModemStatus
                | ATCommandResponse FrameId CommandName CommandStatus [Word8]
                | RemoteATCommandResponse FrameId Address64 Address16 CommandName CommandStatus [Word8]
                | TransmitResponse FrameId TransmitStatus
                | Receive64 Address64 SignalStrength AddressBroadcast PanBroadcast [Word8]
                | Receive16 Address16 SignalStrength AddressBroadcast PanBroadcast [Word8]
                  deriving (Show, Eq)
-- | Commands sent from to computer to the XBee.
data CommandOut = ATCommand FrameId CommandName [Word8]
                | ATQueueCommand FrameId CommandName [Word8]
                | RemoteATCommand64 FrameId Address64 Bool CommandName [Word8]
                | RemoteATCommand16 FrameId Address16 Bool CommandName [Word8]
                | Transmit64 FrameId Address64 DisableAck BroadcastPan [Word8]
                | Transmit16 FrameId Address16 DisableAck BroadcastPan [Word8]
                  deriving (Show, Eq)



instance Serialize CommandName where
    get = liftM CommandName $ liftM2 (,) getWord8 getWord8
    put (CommandName (b1,b2)) = putWord8 b1 >> putWord8 b2
instance Serialize SignalStrength where
    get = liftM SignalStrength getWord8
    put (SignalStrength s) = putWord8 s

getEnumWord8 :: (Enum e) => Get e
getEnumWord8 = liftM (toEnum . fromIntegral) getWord8
putEnumWord8 :: (Enum e) => e -> Put
putEnumWord8 = putWord8 . fromIntegral . fromEnum
instance Serialize ModemStatus where
    get = getEnumWord8
    put = putEnumWord8
instance Serialize CommandStatus where
    get = getEnumWord8
    put = putEnumWord8
instance Serialize TransmitStatus where
    get = getEnumWord8
    put = putEnumWord8

instance Serialize CommandIn where
    put (ModemStatusUpdate s) = putWord8 0x8A >> put s
    put (ATCommandResponse f cmd st d) = putWord8 0x88 >> put f >> put cmd >> put st >> putData d
    put (RemoteATCommandResponse f a64 a16 cmd st d) = putWord8 0x97 >> put f >> put a64 >> put a16 >>
        put cmd >> put st >> putData d
    put (TransmitResponse f st) = putWord8 0x89 >> put f >> put st
    put (Receive64 a ss adbc panbc d) = putWord8 0x80 >> put a >> put ss >> put opts >> putData d
        where opts = (bitOpt 1 adbc) .|. (bitOpt 2 panbc)
    put (Receive16 a ss adbc panbc d) = putWord8 0x81 >> put a >> put ss >> put opts >> putData d
        where opts = (bitOpt 1 adbc) .|. (bitOpt 2 panbc)
    get = getWord8 >>= getCmdIn where
        getCmdIn 0x8A = liftM ModemStatusUpdate get
        getCmdIn 0x88 = liftM4 ATCommandResponse get get get getTillEnd
        getCmdIn 0x97 = RemoteATCommandResponse <$> get <*> get <*> get <*> get <*> get <*> getTillEnd
        getCmdIn 0x89 = liftM2 TransmitResponse get get
        getCmdIn 0x80 = create <$> get <*> get <*> getWord8 <*> getTillEnd
            where create adr ss opts d = Receive64 adr ss (testBit opts 1) (testBit opts 2) d
        getCmdIn 0x81 = create <$> get <*> get <*> getWord8 <*> getTillEnd
            where create adr ss opts d = Receive16 adr ss (testBit opts 1) (testBit opts 2) d
        getCmdIn o    = fail $ "undefined XBee->PC command " ++ show o

instance Serialize CommandOut where
    put (ATCommand f cmd d) = putWord8 0x08 >> put f >> put cmd >> putData d
    get = getWord8 >>= getCmdOut where
        getCmdOut 0x08 = ATCommand <$> get <*> get <*> getTillEnd
        getCmdOut o    = fail $ "undefined PC->XBee command " ++ show o

bitOpt :: Int -> Bool -> Word8
bitOpt i b = if b then (bit i) else 0

getTillEnd :: Get [Word8]
getTillEnd = liftM BS.unpack (remaining >>= getBytes)

putData :: [Word8] -> Put
putData = putByteString . BS.pack
