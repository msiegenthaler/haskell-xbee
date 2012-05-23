module System.Hardware.XBee.Command (
    -- * FrameId
    FrameId,
    frameId,
    nextFrame,
    -- * Command
    CommandId,
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


data ModemStatus = HardwareReset
                 | WatchdogTimerReset
                 | Associated
                 | Disassociated
                 | SyncLost
                 | CoordinatorRealignment
                 | CoordinatorStarted deriving (Enum, Show, Eq, Bounded)

newtype CommandId = CommandId (Word8, Word8)

data CommandStatus = CmdOK
                   | CmdError
                   | CmdInvalidCommand
                   | CmdInvalidParameter deriving (Enum, Show, Eq, Bounded)

-- | Commands to/from the XBee
data Commands = ModemStatusUpdate ModemStatus 
              | ATCommand FrameId CommandId [Word8]
              | ATQueueCommand FrameId CommandId
              | ATCommandResponse FrameId CommandId CommandStatus [Word8]
              -- TODO

instance Serialize FrameId where
    get = liftM FrameId getWord8
    put (FrameId i) = putWord8 i
instance Serialize CommandId where
    get = liftM CommandId $ liftM2 (,) getWord8 getWord8
    put (CommandId (b1,b2)) = putWord8 b1 >> putWord8 b2
instance Serialize ModemStatus where
    get = liftM (toEnum . fromIntegral) getWord8
    put = putWord8 . fromIntegral . fromEnum
instance Serialize CommandStatus where
    get = liftM (toEnum . fromIntegral) getWord8
    put = putWord8 . fromIntegral . fromEnum