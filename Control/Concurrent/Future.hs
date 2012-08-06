module Control.Concurrent.Future (
    Future,
    -- * Construction
    stmFuture,
    forkFuture,
    instantly,
    afterUs,
    -- * Combination
    fasterOf,
    fastestOf,
    -- * Gett
    await,
    awaitAny
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative

-- | A value that becomes available in the future.
newtype Future a = Future (STM a)

instance Monad Future where
    (Future a) >>= f = Future $ a >>= unwrap . f
    return = Future . return
instance Functor Future where
    fmap f (Future a) = Future $ fmap f a
instance Applicative Future where
    pure = return
    fm <*> am = do f <- fm
                   fmap f am

-- | Creates a future from an STM statement.
stmFuture = Future

-- | Executes the IO in a seperate thread (using forkIO) and makes the result
--   available as a future.
forkFuture :: IO a -> IO (Future a)
forkFuture a = do v <- newEmptyTMVarIO
                  forkIO $ a >>= atomically . putTMVar v
                  return $ stmFuture $ takeTMVar v


-- | Returns a future that is instantly set to the supplied value.
instantly :: a -> Future a
instantly = return

-- | Returns a future that gets set to the supplied value after x microseconds.
afterUs :: a -> Int -> IO (Future a)
afterUs a us = registerDelay us >>= return . Future . mkFuture
    where mkFuture v = readTVar v >>= flip unless retry >> return a



-- | Combines two futures into one by taking the one that completes faster.
fasterOf :: Future a -> Future a -> Future a
fasterOf (Future a) (Future b) = Future $ a `orElse` b

-- | Combines a list of futures by taking the result of the one that completes fastests.
-- Fails on an empty list.
fastestOf :: [Future a] -> Future a
fastestOf [] = Future $ error "empty list of futures"
fastestOf (a:[]) = a
fastestOf (a:xs) = fasterOf a (fastestOf xs)



-- | Waits until the future is completed.
await (Future a) = atomically a

-- | Reads the value of the future that completes first. Waits until the first future is
--   available.
awaitAny :: [Future a] -> IO a
awaitAny = await . fastestOf



unwrap (Future a) = a