module Main (main) where

import Test.QuickCheck
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Word
import Data.Serialize
import System.Hardware.XBee.Command
import qualified Data.ByteString as BS
import Control.Monad


ser :: Serialize s => s -> [Word8]
ser = BS.unpack . runPut . put
serParseTest s = (runGet get $ runPut $ put s) == Right s


-- FrameId

frameIdLoopsAroundAfter255 = nextFrame (frameForId 255) == frameId

frameIdSerializeParse :: Word8 -> Bool
frameIdSerializeParse v = serParseTest (frameForId v)

frameIdParseWord8 w = runGet get (BS.singleton w) == Right (frameForId w)

frameForId :: (Num n, Eq n) => n -> FrameId
frameForId 0 = frameId
frameForId i = nextFrame $ frameForId $ i - 1

idForFrame :: FrameId -> Word8
idForFrame = read . drop 1 . dropWhile (/=' ') . show

instance Arbitrary FrameId where
    arbitrary = liftM frameForId (choose (0, 255)::Gen Word8)


-- Modem status

modemStatusSerialize =
        ser HardwareReset == [0]
     && ser WatchdogTimerReset == [1]
     && ser Associated == [2]
     && ser Disassociated == [3]
     && ser SyncLost == [4]
     && ser CoordinatorRealignment == [5]
     && ser CoordinatorStarted == [6]

modemStatusSerializeParse :: ModemStatus -> Bool
modemStatusSerializeParse = serParseTest

instance Arbitrary ModemStatus where
    arbitrary = elements $ enumFrom minBound


-- Command status

commandStatusSerialize =
        ser CmdOK == [0]
     && ser CmdError == [1]
     && ser CmdInvalidCommand == [2]
     && ser CmdInvalidParameter == [3]

commandStatusSerializeParse :: CommandStatus -> Bool
commandStatusSerializeParse = serParseTest

instance Arbitrary CommandStatus where
    arbitrary = elements $ enumFrom minBound



--Main
main = defaultMain tests

tests :: [Test]
tests = [
    testGroup "FrameId" [
        testProperty "loops around after 255" frameIdLoopsAroundAfter255,
        testProperty "serialize and parse yield original value" frameIdSerializeParse,
        testProperty "can parse single Word8" frameIdParseWord8
    ],
    testGroup "ModemStatus" [
        testProperty "values are correctly serialized" modemStatusSerialize,
        testProperty "serialize and then parse yields original value" modemStatusSerializeParse
    ],
    testGroup "CommandStatus" [
        testProperty "values are correctly serialized" commandStatusSerialize,
        testProperty "serialize and then parse yields original value" commandStatusSerializeParse
    ]]
