{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Hardware.XBee.Monad (
    XBeeM,
    runCommand,
    -- * Actions
    fire,
    send,
    setTimeout
) where

import Data.Time.Units
import System.Hardware.XBee.Device (XBee,FrameCmd,CommandHandler,fireCommand,sendCommand)
import System.Hardware.XBee.Command
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.State.Class
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader.Class


-- Monad for XBee commands.
newtype XBeeM a = XBeeM { runXBeeM :: ReaderT XBee (StateT Microsecond IO) a }
    deriving (Monad, MonadIO, MonadReader XBee, MonadState Microsecond)

runCommand :: XBeeM a -> XBee -> IO a
runCommand m x = fst <$> runStateT (runReaderT (runXBeeM m) x) (convertUnit defaultTimeout)

defaultTimeout :: Millisecond
defaultTimeout = 30

-- | Sends a command without waiting for a response.
-- Use noFrameId if the command supports frames.
fire :: CommandOut -> XBeeM ()
fire cmd = ask >>= exec
    where exec x = liftIO $ atomically $ fireCommand x cmd

-- | Sends a command and waits for the response.
-- The timeout set with "setTimeout" does apply.
send :: FrameCmd a -> XBeeM a
send cmd = do
        x   <- ask
        tmo <- get 
        fut <- liftIO $ atomically $ sendCommand x tmo cmd
        liftIO $ atomically fut

-- | Set the timeout for "send".
setTimeout :: TimeUnit time => time -> XBeeM ()
setTimeout = put . convertUnit
