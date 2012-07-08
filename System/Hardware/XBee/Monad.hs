{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Hardware.XBee.Monad (
    XBeeM,
    runCommand,
    Future,
    -- * Actions
    fire,
    send,
    sendAsync,
    afterUs,
    getAsync,
    getFastest,
    fasterOf,
    fastestOf
) where

import Data.Time.Units
import System.Hardware.XBee.Device (XBee,FrameCmd,CommandHandler,fireCommand,sendCommand)
import System.Hardware.XBee.Command
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader.Class


-- Monad for XBee commands.
newtype XBeeM a = XBeeM { runXBeeM :: ReaderT XBee IO a }
    deriving (Monad, MonadIO, MonadReader XBee)

runCommand :: XBeeM a -> XBee -> IO a
runCommand m x = runReaderT (runXBeeM m) x

newtype Future a = Future (STM a)


-- | Sends a command without waiting for a response.
-- Use noFrameId if the command supports frames.
fire :: CommandOut -> XBeeM ()
fire cmd = ask >>= exec
    where exec x = liftIO $ atomically $ fireCommand x cmd

-- | Sends a command and waits for the response.
-- The timeout set with "setTimeout" does apply.
send :: TimeUnit time => time -> FrameCmd a -> XBeeM a
send tmo cmd = do
        x   <- ask
        fut <- liftIO $ atomically $ sendCommand x tmo cmd
        liftIO $ atomically fut

-- | Sends the command and returns a future to receive the result.
-- The timeout set with "setTimeout" does apply.
sendAsync :: TimeUnit time => time -> FrameCmd a -> XBeeM (Future a)
sendAsync tmo cmd = do
        x   <- ask
        fut <- liftIO $ atomically $ sendCommand x tmo cmd
        return $ Future fut

-- | Returns a future that gets set to the supplied value after x microseconds.
afterUs :: a -> Int -> XBeeM (Future a)
afterUs a us = do
        v <- liftIO $ registerDelay us
        return $ Future $ do
            dne <- readTVar v
            unless dne retry
            return a

-- | Reads the value of a future. Waits until the future is available.
getAsync :: Future a -> XBeeM a
getAsync (Future stm) = liftIO $ atomically stm

-- | Reads the value of the future that completes first. Waits until the first future is
-- available.
getFastest :: [Future a] -> XBeeM a
getFastest = getAsync . fastestOf

-- | Combines two futures into one by taking the one that completes faster.
fasterOf :: Future a -> Future a -> Future a
fasterOf (Future a) (Future b) = Future $ a `orElse` b

-- | Combines a list of futures by taking the result of the one that completes fastests.
fastestOf :: [Future a] -> Future a
fastestOf [] = Future $ return undefined
fastestOf (a:[]) = a
fastestOf (a:xs) = fasterOf a (fastestOf xs)
