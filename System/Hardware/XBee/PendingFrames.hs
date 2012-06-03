{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module System.Hardware.XBee.PendingFrames (
    -- * Support classes
    CommandResponse(..),
    CommandHandler,
    -- * Pending Frames
    PendingFrames,
    newPendingFrames,
    enqueue,
    handleCommand
) where

import Control.Concurrent.STM
import Control.Monad
import Data.Maybe
import Data.SouSiT
import System.Hardware.XBee.Command


data CommandResponse = CRData CommandIn
                     | CRPurged
                     | CRTimeout deriving (Show, Eq)

type CommandHandler a = Sink CommandResponse STM a

data PendingFrame = forall a . PendingFrame FrameId (CommandHandler a) (TMVar a)
newtype PendingFrames = PendingFrames (TVar [PendingFrame])

-- | Creates a new instance of PendingFrames
newPendingFrames = liftM PendingFrames (newTVar [])

-- | Enqueue a new pending command.
-- Result: the frame id to use to send the command and the TMVar that will eventually contain
-- the result of the call (as determined by the supplied CommandHandler).
-- Attention: The TMVar must be read in a different atomically block than the one the call to
-- enqueue occurs in.
enqueue :: PendingFrames -> CommandHandler a -> STM (FrameId, TMVar a)
enqueue (PendingFrames fsv) h = do
        fs <- readTVar fsv
        let (fid, tp) = allocateFrameId fs
        purge tp
        v <- newEmptyTMVar
        writeTVar fsv $ fs ++ [PendingFrame fid h v]
        return (fid, v)
    where purge (Just a) = pushCommandResponse a CRPurged >> return ()
          purge Nothing  = return ()

pendingFrameId (PendingFrame f _ _) = f

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


-- | Handles a CommandIn by passing it to the correct command handler.
-- Does not react to commands without a frame id.
handleCommand :: PendingFrames -> CommandIn -> STM ()
handleCommand pf cmd = handleCommand' pf cmd (frameIdFor cmd)

handleCommand' _                   _   Nothing = return ()
handleCommand' (PendingFrames fsv) cmd (Just fid) = do
        fs <- readTVar fsv
        fs' <- foldM push [] fs
        writeTVar fsv fs'
    where push o f | (pendingFrameId f) == fid = pushCommandResponse f (CRData cmd) >>= return . (o ++) . maybeToList
                   | otherwise                 = return (o ++ [f])

pushCommandResponse :: PendingFrame -> CommandResponse -> STM (Maybe PendingFrame)
pushCommandResponse (PendingFrame f h v) i = feedSink h i >>= handleDone
    where handleDone (SinkDone r) = r >>= tryPutTMVar v >> return Nothing
          handleDone h' = return $ Just $ PendingFrame f h' v
