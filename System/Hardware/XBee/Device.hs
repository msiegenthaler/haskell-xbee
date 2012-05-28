{-# LANGUAGE RankNTypes #-}

module System.Hardware.XBee.Device (
    -- * Device
    XBee,
    newDevice
) where

import System.Hardware.XBee.Frame
import System.Hardware.XBee.Command
import Data.Word
import Data.Serialize
import qualified Data.ByteString as BS
import Data.SouSiT
import qualified Data.SouSiT.Trans as T

-- | An XBee device.
data XBee m = XBee (BasicSource m CommandIn) (Sink CommandOut m ())

-- | Creates a new XBee device based upon a byte source and sink.
newDevice :: (Monad m, Source src) => src m Word8 -> Sink Word8 m () -> XBee m
newDevice src sink = XBee src' sink'
    where sink' = T.map commandToFrame =$= frameToWord8 =$ sink
          src'  = src $= word8ToFrame =$= T.map frameToCommand =$= T.eitherRight

commandToFrame :: CommandOut -> Frame
commandToFrame cmd = frame (ser cmd)
    where ser = BS.unpack . runPut . put

frameToCommand :: Frame -> Either String CommandIn
frameToCommand = parse . frameData
    where parse = runGet get . BS.pack

xbeeOut :: Monad m => XBee m -> Sink CommandOut m ()
xbeeOut (XBee _ sink) = sink

xbeeIn :: Monad m => XBee m -> BasicSource m CommandIn
xbeeIn (XBee src _) = src
