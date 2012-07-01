module Control.Concurrent.Latch (
    Latch,
    newCountDownLatch,
    onLatch,
    awaitLatch,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

data Latch = CountDownLatch (TVar Int)

-- | A latch that will will allow the waiting thread(s) to continue as soon as onLatch
--   has been called (at least) n times.
newCountDownLatch :: Integral n => n -> STM Latch
newCountDownLatch = (liftM CountDownLatch) . newTVar . fromIntegral

-- | Signal that the latch has been reached by a thread. Removes one 'token'.
onLatch :: Latch -> STM ()
onLatch (CountDownLatch v) = readTVar v >>= next
    where next n | n > 0     = writeTVar v (n-1)
                 | otherwise = return ()

-- | Wait until the onLatch has been called for the specified times.
awaitLatch (CountDownLatch v) = readTVar v >>= check
    where check 0 = return ()
          check _ = retry
