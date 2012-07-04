-- | Correlates request and responses by means of a Cirular instance.
--   The responses are processed by the ResponseM monad, one request may result in more than
--   one response.

module Control.RequestResponseCorrelator (
    Correlator,
    newCorrelator,
    -- * Request / Push
    request,
    push,
    -- * Response processing
    ResponseM,
    fetch
) where

import Data.Word
import Data.List
import Data.Circular
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative


newtype Id = Id Word64 deriving (Eq,Show)
instance Circular Id where
    initial = Id 0
    next (Id n) | n == maxBound = initial
                | otherwise     = Id (n + 1)

data Entry c i = Entry Id c (TChan i)

data Correlator c i = Correlator { purgeValue :: i,
                                   currentId  :: TVar Id,
                                   inProgress :: TVar [Entry c i] }


-- | Create a new correlator.
newCorrelator :: Circular c =>
    i -- ^ The value sent as a response if the request is purged from the correlator.
    -> IO (Correlator c i)
newCorrelator pv = liftM2 (Correlator pv) (newTVarIO initial) (newTVarIO [])


-- | Monad to handle the response.
-- Use fetch to read a value when inside the monad.
data ResponseM i a = ResponseM (TChan i -> STM a)

instance Monad (ResponseM i) where
    return v = ResponseM (\_ -> return v)
    (ResponseM fa) >>= b = ResponseM f
        where f chan = fa chan >>= wrap . b
                where wrap (ResponseM fb) = fb chan
instance Functor (ResponseM i) where
    fmap fun m = m >>= return . fun
instance Applicative (ResponseM i) where
    pure = return
    a <*> b = do f <- a
                 x <- b
                 return $ f x


-- | Reads a response value. Waits until available.
fetch :: ResponseM i i
fetch = ResponseM $ readTChan


-- | Sends a request and waits for the response.
request :: Circular c => Correlator c i
    -> (c -> IO ())   -- ^ Function used to send the response.
    -> ResponseM i a  -- ^ Monad to process the response.
    -> IO a
request (Correlator pv idV ipV) sf (ResponseM f) = do
        Entry ident key chan <- atomically $ nextId idV >>= addEntry ipV pv
        sf key
        atomically $ f chan <* removeEntry ipV ident

nextId idV = do
        v <- readTVar idV
        writeTVar idV (next v)
        return v

removeEntry inProgressV ident = do
        es <- readTVar inProgressV
        let es' = filter (not . byId ident) es
        writeTVar inProgressV es'
    where byId i (Entry c _ _) = i == c

-- | Create a new entry and add it to the inProgressV. If no slots are available the oldest
-- (=first) request gets fed purgeValue and removed from the inProgressV.
addEntry inProgressV purgeValue ident = do
        chan <- newTChan
        es <- readTVar inProgressV
        let (key,toEvict,es') = allocateKey es
        let entry = Entry ident key chan
        writeTVar inProgressV (es' ++ [entry])
        evict toEvict
        return entry
    where evict Nothing = return ()
          evict (Just (Entry _ _ c)) = writeTChan c purgeValue


-- | Gets a request key to use. If no keys are free then an entry is evicted (snd of return).
allocateKey :: Circular c => [Entry c i] -> (c, Maybe (Entry c i), [Entry c i])
allocateKey es = case findFree keys of 
        (Just k) -> (k, Nothing, es)
        Nothing  -> let evict = head es in (key evict, Just evict, tail es)
    where keys = map key es
          key (Entry _ k _) = k

-- | Let the Correlator process an response.
-- It will forward to response to the correct response processor (ResponseM).
push :: Eq c => Correlator c i -> c -> i -> STM ()
push (Correlator _ _ ipV) key value = do
        es <- readTVar ipV
        process $ find (byKey key) es
    where byKey key (Entry _ k _) = key == k
          process (Just (Entry _ _ c)) = writeTChan c value
          process Nothing = return ()
