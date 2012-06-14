module System.Hardware.XBee.DeviceCommand (
    -- * Types
    CommandHandler,
    CommandResponse(..),
    FrameCmdSpec(..),
    FramelessCmdSpec(..),
) where

import System.Hardware.XBee.Command
import Data.SouSiT
import Control.Monad.Identity


-- * Answers received to a command sent to the XBee.
data CommandResponse = CRData CommandIn
                     | CRPurged
                     | CRTimeout deriving (Show, Eq)

-- * Handler for the answers to a single command sent to the XBee.
type CommandHandler a = Sink CommandResponse Identity a

-- * A command that expects an answer from the XBee.
data FrameCmdSpec a   = FrameCmdSpec (FrameId -> CommandOut) (CommandHandler a)

-- * A command that is sent without checking for an answer. Use noFrameId if the CommandOut
--    supports frames.
data FramelessCmdSpec = FramelessCmdSpec CommandOut
