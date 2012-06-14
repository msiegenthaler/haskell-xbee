module System.Hardware.XBee.DeviceCommand (
    -- * Types
    CommandHandler,
    CommandResponse(..),
    FrameCmdSpec(..),
    FramelessCmdSpec(..),
    -- * Commands
    send
) where

import Data.Word
import Data.SouSiT
import Control.Monad.Identity
import System.Hardware.XBee.Command


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


singleAnswer :: (CommandIn -> a) -> a -> CommandHandler a
singleAnswer f failValue = SinkCont step (return failValue)
    where step (CRData i) = return $ SinkDone (return $ f i)
          step _          = return $ SinkDone (return failValue)


-- Sends up to 100 bytes to another XBee.
send :: XBeeAddress -> [Word8] -> FrameCmdSpec TransmitStatus
send to d = FrameCmdSpec (cmd to) (singleAnswer handler TransmitNoAck)
    where cmd (XBeeAddress64 to) f = Transmit64 f to False False (take 100 d)
          cmd (XBeeAddress16 to) f = Transmit16 f to False False (take 100 d)
          handler (TransmitResponse _ r) = r
          handler _ = TransmitNoAck
