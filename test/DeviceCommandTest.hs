module Main (
    main
) where

import Test.QuickCheck
import Test.Framework
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.XBeeTestSupport

import Data.Word
import Data.SouSiT
import Control.Monad.Identity
import System.Hardware.XBee.Command
import System.Hardware.XBee.DeviceCommand as C


process s v = runIdentity (feedSink s v >>= expectDone)
    where expectDone (SinkDone r) = r
          expectDone _ = error "Sink not done"

sendWithTimeoutIsNoAck a d = let (FrameCmdSpec _ h) = C.send a d in
        process h CRTimeout == TransmitNoAck

sendWithPurgeIsNoAck a d = let (FrameCmdSpec _ h) = C.send a d in
        process h CRPurged == TransmitNoAck

sendWithTransmitStatusIsStatus a d f s = let (FrameCmdSpec _ h) = C.send a d in
        process h (CRData (TransmitResponse f s)) == s



--Main
main = defaultMain tests

tests :: [Test]
tests = [
        testGroup "send" [
            testProperty "with Timeout is a NoAck" sendWithTimeoutIsNoAck,
            testProperty "with Purge is NoAck" sendWithPurgeIsNoAck,
            testProperty "with a TransmitResponse is the status" sendWithTransmitStatusIsStatus
        ]
    ]
