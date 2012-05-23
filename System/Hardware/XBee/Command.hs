module System.Hardware.XBee.Command (
    -- * FrameId
    FrameId,
    frameId,
    nextFrame,
    -- * Address
    Address64(..),
    Address16(..),
    broadcastAddress,
    disabledAddress,
    -- * Command
    CommandName,
    CommandStatus(..),
    ModemStatus(..),
    Commands(..)
) where

import Data.Word
import Data.Serialize
import Control.Monad


newtype FrameId = FrameId Word8 deriving (Show, Eq)
-- | Initial FrameId.
frameId = FrameId 0
-- | The next FrameId. The ids are looped (after FrameId 255 follows FrameId 0)
nextFrame (FrameId i) = FrameId (i+1) --overflow
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

newtype CommandName = CommandName (Word8, Word8)

data CommandStatus = CmdOK
                   | CmdError
                   | CmdInvalidCommand
                   | CmdInvalidParameter deriving (Enum, Show, Eq, Bounded)

-- | Commands to/from the XBee
data Commands = ModemStatusUpdate ModemStatus 
              | ATCommand FrameId CommandName [Word8]
              | ATQueueCommand FrameId CommandName
              | ATCommandResponse FrameId CommandName CommandStatus [Word8]
              | RemoteATCommand64 FrameId Address64 Bool CommandName [Word8]
              | RemoteATCommand16 FrameId Address16 Bool CommandName [Word8]
              | RemoteATCommandResponse FrameId Address64 Address16 CommandName CommandStatus [Word8]
              -- TODO

instance Serialize CommandName where
    get = liftM CommandName $ liftM2 (,) getWord8 getWord8
    put (CommandName (b1,b2)) = putWord8 b1 >> putWord8 b2
instance Serialize ModemStatus where
    get = liftM (toEnum . fromIntegral) getWord8
    put = putWord8 . fromIntegral . fromEnum
instance Serialize CommandStatus where
    get = liftM (toEnum . fromIntegral) getWord8
    put = putWord8 . fromIntegral . fromEnum
