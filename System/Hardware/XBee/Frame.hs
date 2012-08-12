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
    frameToWord8,
    word8ToFrame
) where

import Data.Word
import qualified Data.ByteString as BS
import Data.Serialize
import Control.Monad
import Control.Category ((>>>))
import Data.SouSiT
import qualified Data.SouSiT.Trans as T


-- | A 802.15.4 frame. This is the lowest level of communication with the XBee.
data Frame = Frame [Word8] deriving (Show, Eq)

-- | The maximum length of data within one frame
maxFrameData = 255

-- | Create a frame from the data. All data exceeding maxFrameData is silently discarded.
frame :: [Word8] -> Frame
frame = Frame . take maxFrameData

-- | The data the is contained within the frame.
frameData :: Frame -> [Word8]
frameData (Frame d) = d

-- | Creates as may frames as neccessary to contain all the data.
frames :: [Word8] -> [Frame]
frames [] = []
frames d  = let (h,t) = splitAt maxFrameData d in Frame h:frames t

checksum :: [Word8] -> Word8
checksum d = 0xFF - sum d

startDelimiter :: Word8
startDelimiter = 0x7E

instance Serialize Frame where
    get = do
        del <- getWord8
        when (del /= startDelimiter) $ fail "missing start delimiter"
        len <- getWord16be
        when (fromIntegral len > maxFrameData) $ fail "invalid frame length"
        d <- liftM BS.unpack $ (getBytes . fromIntegral) len
        cs <- getWord8
        when (cs /= checksum d) $ fail "frame checksum is wrong"
        return $ Frame d
    put (Frame d) = do
        putWord8 startDelimiter
        putWord16be (fromIntegral $ length d)
        putByteString $ BS.pack d
        putWord8 $ checksum d

serializeWord8 :: Frame -> [Word8]
serializeWord8 = BS.unpack . runPut . put

deserializeWord8 :: [Word8] -> Either String Frame
deserializeWord8 = runGet get . BS.pack 

-- | Serializes the frames into bytes.
frameToWord8 :: Transform Frame Word8
frameToWord8 = T.map serializeWord8 >>> T.disperse

-- | Reads frames from bytes. Invalid frames (wrong checksum/invalid length) are silently discarded.
word8ToFrame :: Transform Word8 Frame
word8ToFrame = ContTransform (step []) []
    where parse = runGetPartial (get::Get Frame)
          step fs i
                | i == startDelimiter = step' (parse:fs) [] i
                | otherwise           = step' fs         [] i
          step' [] []  _ = ([], word8ToFrame)
          step' [] fs' _ = ([], ContTransform (step fs') [])
          step' (f:fs) fs' i = case f (BS.singleton i) of
                    (Done r _)   -> ([r], word8ToFrame)
                    (Partial f') -> step' fs (fs' ++ [f']) i
                    (Fail _)     -> step' fs fs' i
