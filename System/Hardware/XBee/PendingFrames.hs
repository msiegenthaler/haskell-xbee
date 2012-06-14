{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module System.Hardware.XBee.PendingFrames (
    -- * Support classes
    CommandResponse(..),
    CommandHandler,
    -- * Pending Frames
    PendingFrames,
    newPendingFrames,
    enqueue,
    handleCommand,
    -- * Call result (future)
    XBeeResult,
    resultGet,
    resultTimeout
) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Identity
import Data.Maybe
import Data.SouSiT
import System.Hardware.XBee.Command
import System.Hardware.XBee.DeviceCommand


data PendingFrame = forall a . PendingFrame Integer FrameId (CommandHandler a) (TMVar a)
data PendingFrames = PendingFrames (TVar [PendingFrame]) (TVar Integer)

data XBeeResult a = XBeeResult PendingFrames Integer (TMVar a)

-- | Creates a new instance of PendingFrames
newPendingFrames = liftM2 PendingFrames (newTVar []) (newTVar 0)

-- | Enqueue a new pending command.
-- Result: the frame id to use to send the command and the XBeeResult that will be
-- the result of the call (as determined by the supplied CommandHandler).
-- Attention: The TMVar must be read in a different atomically block than the one the call to
-- enqueue occurs in.
enqueue :: PendingFrames -> CommandHandler a -> STM (FrameId, XBeeResult a)
enqueue pf@(PendingFrames fsv cidv) h = do
        fs <- readTVar fsv
        cid <- incAndGet cidv
        let (fid, tp) = allocateFrameId fs
        purge tp
        v <- newEmptyTMVar
        writeTVar fsv $ fs ++ [PendingFrame cid fid h v]
        return (fid, XBeeResult pf cid v)
    where purge (Just a) = pushCommandResponse a CRPurged >> return ()
          purge Nothing  = return ()

incAndGet :: Enum a => TVar a -> STM a
incAndGet v = do
        i <- readTVar v
        writeTVar v (succ i)
        return i

pendingFrameId (PendingFrame _ f _ _) = f

pendingCallId (PendingFrame i _ _ _) = i

allocateFrameId :: [PendingFrame] -> (FrameId, Maybe PendingFrame)
allocateFrameId fs = case findFreeFrameId fs of
    (Just fid) -> (fid, Nothing)
    Nothing    -> let old = head fs in (pendingFrameId old, Just old)

findFreeFrameId [] = Just frameId
findFreeFrameId pfs = let fs = map pendingFrameId pfs 
                          lst = last fs in
                      check fs lst (nextFrame lst)
    where check fs e c | e == c      = Nothing
                       | c `elem` fs = check fs e (nextFrame c)
                       | otherwise   = Just c


-- | Gets the result of the command.
resultGet :: XBeeResult a -> STM a
resultGet (XBeeResult _ _ v) = takeTMVar v


-- | Times out a command.
resultTimeout :: XBeeResult a -> STM ()
resultTimeout (XBeeResult pf cid _) = processCommand pf predf CRTimeout
    where predf f = (pendingCallId f) == cid


-- | Handles a CommandIn by passing it to the correct command handler.
-- Does not react to commands without a frame id.
handleCommand :: PendingFrames -> CommandIn -> STM ()
handleCommand pf cmd = handleCommand' pf cmd (frameIdFor cmd)

handleCommand' _                     _   Nothing = return ()
handleCommand' (PendingFrames fsv _) cmd (Just fid) = do
        fs <- readTVar fsv
        fs' <- foldM push [] fs
        writeTVar fsv fs'
    where push o f | (pendingFrameId f) == fid = pushCommandResponse f (CRData cmd) >>= return . (o ++) . maybeToList
                   | otherwise                 = return (o ++ [f])

processCommand :: PendingFrames -> (PendingFrame -> Bool) -> CommandResponse -> STM ()
processCommand (PendingFrames fsv _) predf cmd = do
        fs  <- readTVar fsv
        fs' <- foldM push [] fs
        writeTVar fsv fs'
    where push o f | predf f   = pushCommandResponse f cmd >>= return . (o ++) . maybeToList
                   | otherwise = return (o ++ [f])

pushCommandResponse :: PendingFrame -> CommandResponse -> STM (Maybe PendingFrame)
pushCommandResponse (PendingFrame cid f h v) i = handleDone (runIdentity $ feedSink h i)
    where handleDone (SinkDone r) = tryPutTMVar v (runIdentity r) >> return Nothing
          handleDone h' = return $ Just $ PendingFrame cid f h' v
