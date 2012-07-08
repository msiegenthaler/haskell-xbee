{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Hardware.XBee.Monad (
    execute,
    XBeeCmd,
    XBeeCmdAsync,
    Future,
    -- * Actions
    fire,
    send,
    await,
    awaitAny,
    afterUs,
    instantly,
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
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT)


newtype Future a = Future (STM a)

newtype XBeeCmd a = XBeeCmd { runXBeeCmd :: ReaderT XBee IO a }
    deriving (Monad, MonadIO, MonadReader XBee)

type XBeeCmdAsync a = XBeeCmd (Future a)


-- | Execute an async xbee command.
execute :: XBee -> XBeeCmdAsync a -> IO a
execute x m = runReaderT (runXBeeCmd m') x
    where m' = m >>= await


-- | Reads the value of a future. Waits until the future is available.
await :: Future a -> XBeeCmd a
await = liftIO . atomically . unwrapFuture
    where unwrapFuture (Future stm) = stm

-- | Reads the value of the future that completes first. Waits until the first future is
-- available.
awaitAny :: [Future a] -> XBeeCmd a
awaitAny = await . fastestOf


-- | Sends a command without waiting for a response.
-- Use noFrameId if the command supports frames.
fire :: CommandOut -> XBeeCmd ()
fire cmd = ask >>= exec
    where exec x = liftIO $ atomically $ fireCommand x cmd

-- | Sends a command.
send :: TimeUnit time => time -> FrameCmd a -> XBeeCmdAsync a
send tmo cmd = do
        x   <- ask
        fut <- liftIO $ atomically $ sendCommand x tmo cmd
        return $ Future fut

-- | Returns a future that gets set to the supplied value after x microseconds.
afterUs :: a -> Int -> XBeeCmdAsync a
afterUs a us = do
        v <- liftIO $ registerDelay us
        return $ Future $ do
            dne <- readTVar v
            unless dne retry
            return a

-- | Returns a future that is instantly set to the supplied value.
instantly :: a -> XBeeCmdAsync a
instantly = return . Future . return

-- | Combines two futures into one by taking the one that completes faster.
fasterOf :: Future a -> Future a -> Future a
fasterOf (Future a) (Future b) = Future $ a `orElse` b

-- | Combines a list of futures by taking the result of the one that completes fastests.
fastestOf :: [Future a] -> Future a
fastestOf [] = Future $ return undefined
fastestOf (a:[]) = a
fastestOf (a:xs) = fasterOf a (fastestOf xs)
