{-# LANGUAGE NoMonomorphismRestriction #-}

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

parse :: Serialize s => [Word8] -> Either String s
parse = runGet get . BS.pack

-- FrameId

frameIdLoopsAroundAfter255 = nextFrame (frameForId 255) == frameId

frameIdSerializeParse :: Word8 -> Bool
frameIdSerializeParse v = serParseTest (frameForId v)

frameIdParseWord8 w = runGet get (BS.singleton w) == Right (frameForId w)

frameForId :: (Num n, Eq n) => n -> FrameId
frameForId 0 = noFrameId
frameForId 1 = frameId
frameForId i = nextFrame $ frameForId $ i - 1

idForFrame :: FrameId -> Word8
idForFrame = read . drop 1 . dropWhile (/=' ') . show

instance Arbitrary FrameId where
    arbitrary = liftM frameForId (choose (0, 255)::Gen Word8)


-- Command name
commandNameSerializeParse a b = serParseTest (commandName a b)

commandNameExampleDH = parse [0x44, 0x48] == Right (commandName 'D' 'H')

instance Arbitrary CommandName where
    arbitrary = liftM2 commandName letter letter
        where letter = choose ('A', 'Z')

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


-- Address

address64SerializeParse :: Address64 -> Bool
address64SerializeParse = serParseTest

address16SerializeParse :: Address16 -> Bool
address16SerializeParse = serParseTest

instance Arbitrary Address64 where
    arbitrary = liftM Address64 (arbitrary :: Gen Word64)

instance Arbitrary Address16 where
    arbitrary = liftM Address16 (arbitrary :: Gen Word16)


-- Transmit status

transmitStatusSerialize =
        ser TransmitSuccess == [0]
     && ser TransmitNoAck == [1]
     && ser TransmitCcaFailure == [2]
     && ser TransmitPurged == [3]

transmitStatusSerializeParse :: TransmitStatus -> Bool
transmitStatusSerializeParse = serParseTest

instance Arbitrary TransmitStatus where
    arbitrary = elements $ enumFrom minBound


-- Signal Strength

signalStrengthSerializeParse :: SignalStrength -> Bool
signalStrengthSerializeParse s = serParseTest s

signalStrengthExample = parse [0x28] == Right (fromDbm (-40))

instance Arbitrary SignalStrength where
    arbitrary = liftM fromDbm (choose ((-255), 0))


-- Command In

modemStatusUpdateSerializeParse s = serParseTest (ModemStatusUpdate s)

modemStatusUpdateParseExample = parse [0x8A, 0x01] == Right (ModemStatusUpdate WatchdogTimerReset)

atCommandResponseSerializeParse f cmd st val = serParseTest (ATCommandResponse f cmd st val)

atCommandResponseExample = parse [0x88, 0x52, 0x4D, 0x59, 0x00, 0x23, 0x12] ==
        Right (ATCommandResponse (frameForId 0x52) (commandName 'M' 'Y') CmdOK [0x23, 0x12])

remoteAtCommandResponseSerializeParse f a64 a16 cmd st val = serParseTest $
    RemoteATCommandResponse f a64 a16 cmd st val

remoteAtCommandResponseExample = parse [0x97, 0x52,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF,
            0xFF, 0xFE,
            0x4D, 0x59, 0x00, 0x23, 0x12] ==
        Right (RemoteATCommandResponse (frameForId 0x52) broadcastAddress disabledAddress (commandName 'M' 'Y') CmdOK [0x23, 0x12])

transmitResponseSerializeParse f s = serParseTest (TransmitResponse f s)

transmitResponseExample = parse [0x89, 0x10, 0x00] ==
        Right (TransmitResponse (frameForId 0x10) TransmitSuccess)

receive64SerializeParse from ss ack bc d = serParseTest $ Receive64 from ss ack bc d

receive64Example :: [Word8] -> Bool
receive64Example d = parse ([0x80,
            0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
            0x28, 0x00] ++ d) ==
        Right (Receive64 (Address64 0x0102030405060708) (fromDbm (-40)) False False d)

receive16SerializeParse from ss ack bc d = serParseTest $ Receive16 from ss ack bc d

receive16Example :: [Word8] -> Bool
receive16Example d = parse ([0x81,
            0x01, 0x02,
            0x28, 0x00] ++ d) ==
        Right (Receive16 (Address16 0x0102) (fromDbm (-40)) False False d)


-- Command Out

atCommandSerializeParse f cmd d = serParseTest (ATCommand f cmd d)

atCommandExample = parse [0x08, 0x01, 0x44, 0x4C, 0x05, 0x06, 0x07, 0x08] ==
        Right (ATCommand (frameForId 1) (commandName 'D' 'L') [5, 6, 7, 8])

atQueueCommandSerializeParse f cmd d = serParseTest (ATQueueCommand f cmd d)

atQueueCommandExample = parse [0x09, 0x01, 0x44, 0x4C, 0x05, 0x06, 0x07, 0x08] ==
        Right (ATQueueCommand (frameForId 1) (commandName 'D' 'L') [5, 6, 7, 8])

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
    testGroup "CommandName" [
        testProperty "serialize and then parse yields original value" commandNameSerializeParse,
        testProperty "value DH is parsed correctly" commandNameExampleDH
    ],
    testGroup "CommandStatus" [
        testProperty "values are correctly serialized" commandStatusSerialize,
        testProperty "serialize and then parse yields original value" commandStatusSerializeParse
    ],
    testGroup "SignalStrength" [
        testProperty "serialize and then parse yields original value" signalStrengthSerializeParse,
        testProperty "value -40 (=0x28) is parsed correctly" signalStrengthExample
    ],
    testGroup "TransmitStatus" [
        testProperty "values are correctly serialized" transmitStatusSerialize,
        testProperty "serialize and then parse yields original value" transmitStatusSerializeParse
    ],
    testGroup "Address64" [
        testProperty "serialize and then parse yields original value" address64SerializeParse
    ],
    testGroup "Address16" [
        testProperty "serialize and then parse yields original value" address16SerializeParse
    ],
    testGroup "CommandIn" [
        testProperty "ModemStatusUpdate serialize & parse yields original" modemStatusUpdateSerializeParse,
        testProperty "ModemStatusUpdate example works" modemStatusUpdateParseExample,
        testProperty "ATCommandResponse serialize & parse yields original" atCommandResponseSerializeParse,
        testProperty "ATCommandResponse example works" atCommandResponseExample,
        testProperty "RemoteATCommandResponse serialize & parse yields original" remoteAtCommandResponseSerializeParse,
        testProperty "RemoteATCommandResponse example works" remoteAtCommandResponseExample,
        testProperty "TransmitResponse serialize & parse yields original" transmitResponseSerializeParse,
        testProperty "TransmitResponse example works" transmitResponseExample,
        testProperty "Receive64 serialize & parse yields original" receive64SerializeParse,
        testProperty "Receive64 example works" receive64Example,
        testProperty "Receive16 serialize & parse yields original" receive16SerializeParse,
        testProperty "Receive16 example works" receive16Example
    ],
    testGroup "CommandOut" [
        testProperty "ATCommand serialize & parse yields original" atCommandSerializeParse,
        testProperty "ATCommand example works" atCommandExample,
        testProperty "ATQueueCommand serialize & parse yields original" atQueueCommandSerializeParse,
        testProperty "ATQueueCommand example works" atQueueCommandExample
    ]]
