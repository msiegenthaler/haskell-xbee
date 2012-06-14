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

runHandler cmd d = let (FrameCmdSpec _ h) = cmd in process h d
runHandler' cmd d = runHandler cmd (CRData d)

sendWithTimeoutIsNoAck a d = runHandler (C.send a d) CRTimeout == TransmitNoAck

sendWithPurgeIsNoAck a d = runHandler (C.send a d) CRPurged == TransmitNoAck

sendWithTransmitStatusIsStatus a d f s = runHandler' (C.send a d) (TransmitResponse f s) == s


runAtHandler dest req = runHandler (C.atCommand dest req)
runAtHandler' dest req resp = runAtHandler dest req (CRData resp)

atCommandLocal cn d f s out =
    runAtHandler' Local (ATRequest cn d) (ATCommandResponse f cn s out) == ATResponse s out

atCommandLocalTimeout cn d =
    runAtHandler Local (ATRequest cn d) CRTimeout == ATResponse CmdError []

atCommandLocalPurge cn d =
    runAtHandler Local (ATRequest cn d) CRPurged == ATResponse CmdError []

atCommandRemote a cn d f s out = runAtHandler' (Remote a) (ATRequest cn d) 
        (RemoteATCommandResponse f a64 a16 cn s out) == ATResponse s out
    where a16 = case a of
                    (XBeeAddress16 a') -> a'
                    _ -> disabledAddress
          a64 = case a of
                    (XBeeAddress64 a') -> a'
                    _ -> broadcastAddress

atCommandRemoteTimeout a cn d =
    runAtHandler (Remote a) (ATRequest cn d) CRTimeout == ATResponse CmdError []

atCommandRemotePurge a cn d =
    runAtHandler (Remote a) (ATRequest cn d) CRPurged == ATResponse CmdError []






--Main
main = defaultMain tests

tests :: [Test]
tests = [
        testGroup "send" [
            testProperty "with Timeout is a NoAck" sendWithTimeoutIsNoAck,
            testProperty "with Purged is NoAck" sendWithPurgeIsNoAck,
            testProperty "with a TransmitResponse is the status" sendWithTransmitStatusIsStatus
        ],
        testGroup "atCommand" [
            testProperty "local with a ATCommandResponse has data and status" atCommandLocal,
            testProperty "local with Timeout is CmdError and no data" atCommandLocalPurge,
            testProperty "local with Purged is CmdError and no data" atCommandLocalPurge,
            testProperty "remote with a ATCommandResponse has data and status" atCommandRemote,
            testProperty "remote with Timeout is CmdError and no data" atCommandRemotePurge,
            testProperty "remote with Purged is CmdError and no data" atCommandRemotePurge
        ]
    ]
