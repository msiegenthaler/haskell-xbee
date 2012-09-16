module System.Hardware.XBee.Frame (
    -- * Construction
    Frame,
    maxFrameData,
    frame,
    frameData,
    frames,
    -- * Serialize
    get,
    put,
    -- * SouSiT Transforms
    encodeFrame,
    decodeFrame
) where

import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize
import Control.Monad
import Data.SouSiT
import qualified Data.SouSiT.Trans as T


-- | A 802.15.4 frame. This is the lowest level of communication with the XBee.
data Frame = Frame ByteString deriving (Show, Eq)

-- | The maximum length of data within one frame
maxFrameData = 255

-- | Create a frame from the data. All data exceeding maxFrameData is silently discarded.
frame :: ByteString -> Frame
frame = Frame . BS.take maxFrameData

-- | The data the is contained within the frame.
frameData :: Frame -> ByteString
frameData (Frame d) = d

-- | Creates as may frames as neccessary to contain all the data.
frames :: ByteString -> [Frame]
frames d | BS.null d = []
         | otherwise = let (h,t) = BS.splitAt maxFrameData d in Frame h:frames t

checksum :: ByteString -> Word8
checksum d = BS.foldl (-) 0xFF d

startDelimiter :: Word8
startDelimiter = 0x7E

instance Serialize Frame where
    get = do
        del <- getWord8
        when (del /= startDelimiter) $ fail "missing start delimiter"
        len <- getWord16be
        when (fromIntegral len > maxFrameData) $ fail "invalid frame length"
        d <- (getBytes . fromIntegral) len
        cs <- getWord8
        when (cs /= checksum d) $ fail "frame checksum is wrong"
        return $ Frame d
    put (Frame d) = do
        putWord8 startDelimiter
        putWord16be (fromIntegral $ BS.length d)
        putByteString d
        putWord8 $ checksum d


-- | Serializes the frames into bytes.
encodeFrame :: Transform Frame ByteString
encodeFrame = T.serialize

-- | Reads frames from bytes. Invalid frames (wrong checksum/invalid length) are silently discarded.
decodeFrame :: Transform ByteString Frame
decodeFrame = T.map (breakAllByte startDelimiter) . T.disperse . T.deserialize

-- | Splits the ByteString into fragments that all start with 'at'.
breakAllByte :: Word8 -> ByteString -> [ByteString]
breakAllByte at bs | BS.null bs = []
                   | otherwise  = let (a,b) = BS.breakByte at (BS.tail bs) in
                                  let a' = BS.cons (BS.head bs) a in
                    if BS.null b then [a'] else  a':(breakAllByte at b)
