module System.Hardware.XBee.Frame (
    -- * Construction
    Frame,
    maxFrameData,
    frame,
    frames,
    -- * Serialize
    get,
    put,
    -- * SouSiT Transforms
    frameToWord8
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
frame = Frame . (take maxFrameData)

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
        d <- (liftM BS.unpack $ (getBytes . fromIntegral) len)
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

frameToWord8 :: Transform Frame Word8
frameToWord8 = T.map serializeWord8 >>> T.disperse
