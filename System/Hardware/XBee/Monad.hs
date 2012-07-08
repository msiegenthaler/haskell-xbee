{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Hardware.XBee.Monad (
    execute,
    execute',
    XBeeCmd,
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
newtype XBeeCmd a = XBeeCmd { runXBeeCmd :: ReaderT XBee IO a }
    deriving (Monad, MonadIO, MonadReader XBee)

runCommand :: XBeeCmd a -> XBee -> IO a
runCommand m x = runReaderT (runXBeeCmd m) x

-- | Executes an xbee command on the XBee.
-- Waits until the result of the command is available.
execute = flip runCommand

-- | Executes an xbee command on the XBee.
-- Waits until the result of the command is available.
execute' x m = execute x (m >>= getAsync)

newtype Future a = Future (STM a)


-- | Sends a command without waiting for a response.
-- Use noFrameId if the command supports frames.
fire :: CommandOut -> XBeeCmd ()
fire cmd = ask >>= exec
    where exec x = liftIO $ atomically $ fireCommand x cmd

-- | Sends a command and waits for the response.
-- The timeout set with "setTimeout" does apply.
send :: TimeUnit time => time -> FrameCmd a -> XBeeCmd a
send tmo cmd = do
        x   <- ask
        fut <- liftIO $ atomically $ sendCommand x tmo cmd
        liftIO $ atomically fut

-- | Sends the command and returns a future to receive the result.
-- The timeout set with "setTimeout" does apply.
sendAsync :: TimeUnit time => time -> FrameCmd a -> XBeeCmd (Future a)
sendAsync tmo cmd = do
        x   <- ask
        fut <- liftIO $ atomically $ sendCommand x tmo cmd
        return $ Future fut

-- | Returns a future that gets set to the supplied value after x microseconds.
afterUs :: a -> Int -> XBeeCmd (Future a)
afterUs a us = do
        v <- liftIO $ registerDelay us
        return $ Future $ do
            dne <- readTVar v
            unless dne retry
            return a

-- | Reads the value of a future. Waits until the future is available.
getAsync :: Future a -> XBeeCmd a
getAsync (Future stm) = liftIO $ atomically stm

-- | Reads the value of the future that completes first. Waits until the first future is
-- available.
getFastest :: [Future a] -> XBeeCmd a
getFastest = getAsync . fastestOf

-- | Combines two futures into one by taking the one that completes faster.
fasterOf :: Future a -> Future a -> Future a
fasterOf (Future a) (Future b) = Future $ a `orElse` b

-- | Combines a list of futures by taking the result of the one that completes fastests.
fastestOf :: [Future a] -> Future a
fastestOf [] = Future $ return undefined
fastestOf (a:[]) = a
fastestOf (a:xs) = fasterOf a (fastestOf xs)
