module System.Hardware.XBee.DeviceCommand (
    -- * Types
    CommandHandler,
    CommandResponse(..),
    FrameCmdSpec(..),
    FramelessCmdSpec(..),
    -- * Commands
    -- ** Sending
    send,
    sendNoAck,
    broadcast,
    -- ** Generic AT
    ATRequest(..),
    ATResponse(..),
    ATDestination(..),
    atCommand
) where

import Data.Word
import Data.SouSiT
import Control.Monad.Identity
import System.Hardware.XBee.Command


-- | Answers received to a command sent to the XBee.
data CommandResponse = CRData CommandIn
                     | CRPurged
                     | CRTimeout deriving (Show, Eq)

-- | Handler for the answers to a single command sent to the XBee.
type CommandHandler a = Sink CommandResponse Identity a

-- | A command that expects an answer from the XBee.
data FrameCmdSpec a   = FrameCmdSpec (FrameId -> CommandOut) (CommandHandler a)

-- | A command that is sent without checking for an answer. Use noFrameId if the CommandOut
--   supports frames.
data FramelessCmdSpec = FramelessCmdSpec CommandOut


singleAnswer :: (CommandIn -> a) -> a -> CommandHandler a
singleAnswer f failValue = SinkCont step (return failValue)
    where step (CRData i) = return $ SinkDone (return $ f i)
          step _          = return $ SinkDone (return failValue)


transmit f (XBeeAddress64 a) noack bdcst d = Transmit64 f a noack bdcst (take 100 d)
transmit f (XBeeAddress16 a) noack bdcst d = Transmit16 f a noack bdcst (take 100 d)

-- | Sends up to 100 bytes to another XBee and requests an ack.
send :: XBeeAddress -> [Word8] -> FrameCmdSpec TransmitStatus
send to d = FrameCmdSpec cmd (singleAnswer handler TransmitNoAck)
    where cmd f = transmit f to False False d
          handler (TransmitResponse _ r) = r
          handler _ = TransmitNoAck

-- | Sends up to 100 bytes to another XBee without requesting an acknowledgement. There
--   is no way to tell whether the transmission succeeded or not.
sendNoAck :: XBeeAddress -> [Word8] -> FramelessCmdSpec
sendNoAck a d = FramelessCmdSpec $ transmit noFrameId a True False d

-- | Broadcasts up to 100 bytes to all XBees within the same network.
broadcast :: [Word8] -> FramelessCmdSpec
broadcast d = FramelessCmdSpec $ transmit noFrameId (XBeeAddress64 broadcastAddress) True True d


data ATRequest = ATRequest CommandName [Word8] deriving (Eq, Show)
data ATResponse = ATResponse CommandStatus [Word8] deriving (Eq, Show)

data ATDestination = Local
                   | Remote XBeeAddress deriving (Eq, Show)

-- | Generic implementation for the handling of ATCommands with a single response and immediate
--   application.
atCommand :: ATDestination -> ATRequest -> FrameCmdSpec ATResponse
atCommand Local (ATRequest cn d) = FrameCmdSpec cmd (singleAnswer handler atCommandFailed)
    where cmd f = ATCommand f cn d
          handler (ATCommandResponse _ _ s out) = ATResponse s out
          handler _ = atCommandFailed
atCommand (Remote a) (ATRequest cn d) = FrameCmdSpec (cmd a) (singleAnswer handler atCommandFailed)
    where cmd (XBeeAddress64 a64) f = RemoteATCommand64 f a64 True cn d
          cmd (XBeeAddress16 a16) f = RemoteATCommand16 f a16 True cn d
          handler (RemoteATCommandResponse _ _ _ _ s out) = ATResponse s out
          handler _ = atCommandFailed

atCommandFailed = ATResponse CmdError []
