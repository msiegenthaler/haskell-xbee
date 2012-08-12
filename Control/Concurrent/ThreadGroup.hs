module Control.Concurrent.ThreadGroup (
    ThreadGroup,
    startThreadGroup,
    awaitThreadGroup,
    killThreadGroup
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Latch
import Control.Exception
import Control.Monad


data ThreadGroup = ThreadGroup (MVar (Maybe SomeException)) [ThreadId]

-- | Starts every IO from the input in a separate thread (using forkIO).
--   If one thread terminates with an exception then all other threads are killed and
--   the execution is terminated.
startThreadGroup :: [IO ()] -> IO ThreadGroup
startThreadGroup actions = do
        latch <- atomically $ newCountDownLatch $ length actions
        errorBox <- newEmptyTMVarIO
        ts <- mapM (forkIO . wrapExec latch errorBox) actions
        done <- newEmptyMVar
        forkIO $ do r <- atomically $ watchAndWait latch errorBox
                    handleResult ts r
                    putMVar done r
        return $ ThreadGroup done ts
    where 
        watchAndWait l e = (awaitLatch l >> return Nothing) `orElse` liftM Just (takeTMVar e)
        handleResult ts (Just e) = mapM_ killThread ts
        handleResult _  Nothing  = return ()

wrapExec doneLatch errorBox action = handle onError $ action >> onDone
    where onDone = atomically (onLatch doneLatch)        
          onError e@(SomeException _) = void $ atomically (tryPutTMVar errorBox e)


-- | Wait for all threads of the group to terminate. If a thread threw an exeception then this
--   exception is rethrown here
awaitThreadGroup :: ThreadGroup -> IO ()
awaitThreadGroup (ThreadGroup d _) = readMVar d >>= handle
    where handle Nothing = return ()
          handle (Just e) = throwIO e

-- | Kills all threads in the thread group. This will result in awaitThreadGroup to throw 
--   a thread killed.
killThreadGroup :: ThreadGroup -> IO ()
killThreadGroup (ThreadGroup _ ts) = mapM_ killThread ts
