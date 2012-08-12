module Test.XBeeTestSupport (
    arbitrary,
    idForFrame,
    frameForId
) where

import Test.QuickCheck
import Data.Word
import Control.Monad
import System.Hardware.XBee.Command


instance Arbitrary CommandName where
    arbitrary = liftM2 commandName letter letter
        where letter = choose ('A', 'Z')


frameForId :: (Num n, Eq n) => n -> FrameId
frameForId 0 = noFrameId
frameForId 1 = frameId
frameForId i = nextFrame $ frameForId $ i - 1

idForFrame :: FrameId -> Word8
idForFrame = read . drop 1 . dropWhile (/=' ') . show

instance Arbitrary FrameId where
    arbitrary = liftM frameForId (choose (0, 255)::Gen Word8)

instance Arbitrary ModemStatus where
    arbitrary = elements $ enumFrom minBound

instance Arbitrary CommandStatus where
    arbitrary = elements $ enumFrom minBound


instance Arbitrary Address64 where
    arbitrary = liftM Address64 (arbitrary :: Gen Word64)

instance Arbitrary Address16 where
    arbitrary = liftM Address16 (arbitrary :: Gen Word16)

instance Arbitrary XBeeAddress where
    arbitrary = arbitrary >>= create
        where create True = liftM XBeeAddress64 arbitrary 
              create False = liftM XBeeAddress16 arbitrary

instance Arbitrary TransmitStatus where
    arbitrary = elements $ enumFrom minBound

instance Arbitrary SignalStrength where
    arbitrary = liftM fromDbm (choose (-255, 0))

