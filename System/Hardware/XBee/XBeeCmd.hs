{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Hardware.XBee.XBeeCmd (
    execute,
    execute',
    XBeeCmd,
    XBeeCmdAsync,
    Future,
    -- * Actions
    fire,
    send,
    -- * Reexports
    liftIO,
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
import Control.Concurrent.Future
import Control.Concurrent.STM
import Control.Applicative
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT)


newtype XBeeCmd a = XBeeCmd { runXBeeCmd :: ReaderT XBee IO a }
    deriving (Monad, MonadIO, MonadReader XBee, Functor, Applicative)

type XBeeCmdAsync a = XBeeCmd (Future a)


-- | Execute an async xbee command.
execute :: XBee -> XBeeCmdAsync a -> IO a
execute x m = runReaderT (runXBeeCmd m) x >>= await

-- | Execute an async xbee command and catches any exception that occured.
execute' :: XBee -> XBeeCmdAsync a -> IO (Either String a)
execute' x m = E.catch (liftM Right $ execute x m) (return . Left . showE)
    where showE :: SomeException -> String
          showE = show

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
        return $ stmFuture fut
