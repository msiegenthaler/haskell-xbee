module System.Hardware.XBee.DeviceCommand (
    FrameCmdSpec(..),
    FramelessCmdSpec(..),
    CommandHandler,
    CommandResponse(..)
) where

import System.Hardware.XBee.Command
import Data.SouSiT
import Control.Monad.Identity


data CommandResponse = CRData CommandIn
                     | CRPurged
                     | CRTimeout deriving (Show, Eq)

type CommandHandler a = Sink CommandResponse Identity a

data FrameCmdSpec a   = FrameCmdSpec (FrameId -> CommandOut) (CommandHandler a)
data FramelessCmdSpec = FramelessCmdSpec CommandOut
