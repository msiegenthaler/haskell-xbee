{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Test.QuickCheck
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.XBeeTestSupport

import Data.Word
import Data.ByteString (ByteString,pack,unpack)
import qualified Data.ByteString as BS
import Data.Serialize
import System.Hardware.XBee.Command
import Control.Monad


ser :: Serialize s => s -> [Word8]
ser = BS.unpack . runPut . put

serParseTest s = runGet get (runPut $ put s) == Right s

parse :: Serialize s => [Word8] -> Either String s
parse = runGet get . BS.pack

-- FrameId

frameIdLoopsAroundAfter255 = nextFrame (frameForId 255) == frameId

frameIdSerializeParse :: Word8 -> Bool
frameIdSerializeParse v = serParseTest (frameForId v)

frameIdParseWord8 w = runGet get (BS.singleton w) == Right (frameForId w)


-- Command name
commandNameSerializeParse a b = serParseTest (commandName a b)

commandNameExampleDH = parse [0x44, 0x48] == Right (commandName 'D' 'H')


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


-- Command status

commandStatusSerialize =
        ser CmdOK == [0]
     && ser CmdError == [1]
     && ser CmdInvalidCommand == [2]
     && ser CmdInvalidParameter == [3]

commandStatusSerializeParse :: CommandStatus -> Bool
commandStatusSerializeParse = serParseTest


-- Address

address64SerializeParse :: Address64 -> Bool
address64SerializeParse = serParseTest

address16SerializeParse :: Address16 -> Bool
address16SerializeParse = serParseTest


-- Transmit status

transmitStatusSerialize =
        ser TransmitSuccess == [0]
     && ser TransmitNoAck == [1]
     && ser TransmitCcaFailure == [2]
     && ser TransmitPurged == [3]

transmitStatusSerializeParse :: TransmitStatus -> Bool
transmitStatusSerializeParse = serParseTest


-- Signal Strength

signalStrengthSerializeParse :: SignalStrength -> Bool
signalStrengthSerializeParse = serParseTest

signalStrengthExample = parse [0x28] == Right (fromDbm (-40))


-- Command In

modemStatusUpdateSerializeParse s = serParseTest (ModemStatusUpdate s)

modemStatusUpdateParseExample = parse [0x8A, 0x01] == Right (ModemStatusUpdate WatchdogTimerReset)

atCommandResponseSerializeParse f cmd st val = serParseTest (ATCommandResponse f cmd st val)

atCommandResponseExample = parse [0x88, 0x52, 0x4D, 0x59, 0x00, 0x23, 0x12] ==
        Right (ATCommandResponse (frameForId 0x52) (commandName 'M' 'Y') CmdOK $ pack [0x23, 0x12])

remoteAtCommandResponseSerializeParse f a64 a16 cmd st val = serParseTest $
    RemoteATCommandResponse f a64 a16 cmd st val

remoteAtCommandResponseExample = parse [0x97, 0x52,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF,
            0xFF, 0xFE,
            0x4D, 0x59, 0x00, 0x23, 0x12] ==
        Right (RemoteATCommandResponse (frameForId 0x52) broadcastAddress disabledAddress (commandName 'M' 'Y') CmdOK $ pack [0x23, 0x12])

transmitResponseSerializeParse f s = serParseTest (TransmitResponse f s)

transmitResponseExample = parse [0x89, 0x10, 0x00] ==
        Right (TransmitResponse (frameForId 0x10) TransmitSuccess)

receive64SerializeParse from ss ack bc d = serParseTest $ Receive64 from ss ack bc d

receive64Example :: ByteString -> Bool
receive64Example d = parse ([0x80,
            0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
            0x28, 0x00] ++ unpack d) ==
        Right (Receive64 (Address64 0x0102030405060708) (fromDbm (-40)) False False d)

receive16SerializeParse from ss ack bc d = serParseTest $ Receive16 from ss ack bc d

receive16Example :: ByteString -> Bool
receive16Example d = parse ([0x81,
            0x01, 0x02,
            0x28, 0x00] ++ unpack d) ==
        Right (Receive16 (Address16 0x0102) (fromDbm (-40)) False False d)


-- Command Out

someData = pack [5..8]

atCommandSerializeParse f cmd d = serParseTest (ATCommand f cmd d)

atCommandExample = parse [0x08, 0x01, 0x44, 0x4C, 0x05, 0x06, 0x07, 0x08] ==
        Right (ATCommand (frameForId 1) (commandName 'D' 'L') someData)

atQueueCommandSerializeParse f cmd d = serParseTest (ATQueueCommand f cmd d)

atQueueCommandExample = parse [0x09, 0x01, 0x44, 0x4C, 0x05, 0x06, 0x07, 0x08] ==
        Right (ATQueueCommand (frameForId 1) (commandName 'D' 'L') someData)

remoteAtCommand64SerializeParse f adr b cmd d = serParseTest (RemoteATCommand64 f adr b cmd d)

remoteAtCommand64Example = parse [0x17, 0x01,
            0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
            0xFF, 0xFE,
            0x02,
            0x44, 0x4C,
            0x05, 0x06, 0x07, 0x08] ==
        Right (RemoteATCommand64 (frameForId 1) (Address64 0x0102030405060708) True (commandName 'D' 'L') someData)

remoteAtCommand16SerializeParse f adr b cmd d = serParseTest (RemoteATCommand16 f adr b cmd d)

remoteAtCommand16Example = parse [0x17, 0x01,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF,
            0x01, 0x02,
            0x02,
            0x44, 0x4C,
            0x05, 0x06, 0x07, 0x08] ==
        Right (RemoteATCommand16 (frameForId 1) (Address16 0x0102) True (commandName 'D' 'L') someData)

transmit64SerializeParse f adr da bc d = serParseTest (Transmit64 f adr da bc d)

transmit64Example = parse [0x00, 0x02,
            0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
            0x00,
            0x09, 0x08, 0x07] ==
        Right (Transmit64 (frameForId 2) (Address64 0x0102030405060708) False False $ pack [0x09, 0x08, 0x07])

transmit16SerializeParse f adr da bc d = serParseTest (Transmit16 f adr da bc d)

transmit16Example = parse [0x01, 0x03,
            0x01, 0x02,
            0x00,
            0x09, 0x08, 0x07] ==
        Right (Transmit16 (frameForId 3) (Address16 0x0102) False False $ pack [0x09, 0x08, 0x07])

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
        testProperty "ATQueueCommand example works" atQueueCommandExample,
        testProperty "RemoteATCommand64 serialize & parse yields original" remoteAtCommand64SerializeParse,
        testProperty "RemoteATCommand64 example works" remoteAtCommand64Example,
        testProperty "RemoteATCommand16 serialize & parse yields original" remoteAtCommand16SerializeParse,
        testProperty "RemoteATCommand16 example works" remoteAtCommand16Example,
        testProperty "Transmit64 serialize & parse yields original" transmit64SerializeParse,
        testProperty "Transmit64 example works" transmit64Example,
        testProperty "Transmit16 serialize & parse yields original" transmit16SerializeParse,
        testProperty "Transmit16 example works" transmit16Example
    ]]
