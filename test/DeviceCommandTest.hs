module Main (
    main
) where

import Test.QuickCheck
import Test.Framework
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.XBeeTestSupport
import Data.Word
import Data.Maybe
import Control.RequestResponseCorrelator
import Control.Monad
import Control.Monad.State
import Control.Applicative
import System.Hardware.XBee.Command
import System.Hardware.XBee.DeviceCommand as C

readElem = liftM unwrap $ get <* put Nothing
    where unwrap (Just a) = a
          unwrap Nothing = error "Reading more than one element."

process :: CommandHandler a -> CommandResponse -> a
process h v = let (r,s) = runState (processResponse h readElem) (Just v) in
        if isJust s then error "No element read" else r

runHandler (FrameCmdSpec _ h) d = process h d
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


instance Arbitrary ATDestination where
    arbitrary = arbitrary >>= create
        where create True = return Local
              create False = liftM Remote arbitrary


runAtOk :: FrameId -> FrameCmdSpec a -> [Word8] -> a
runAtOk frameId (FrameCmdSpec cmd h) d = case ic of
        (ATCommand f cn _)             -> result $ ATCommandResponse f cn CmdOK d
        (RemoteATCommand16 f a _ cn _) -> result $ RemoteATCommandResponse f broadcastAddress a cn CmdOK d
        (RemoteATCommand64 f a _ cn _) -> result $ RemoteATCommandResponse f a disabledAddress cn CmdOK d
    where ic = cmd frameId
          result = process h . CRData


atMyReadValid f dst = runAtOk f (readAT C.address16 dst) [0x21, 0x0E] == Right (Address16 0x210E)

atMyWriteValid f = (cmd f) == ATCommand f (commandName 'M' 'Y') [0x33, 0x21]
    where (FrameCmdSpec cmd _) = setAT C.address16 Local (Address16 0x3321)


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
        ],
        testGroup "AT MY" [
            testProperty "getAddress16 should return the address on valid responses" atMyReadValid,
            testProperty "setAddress16 should write the address to the request" atMyWriteValid
        ]
    ]
