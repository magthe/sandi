-- |
-- Module     : Data.ByteString.Iteratee.Internals
-- Copyright  : (c) Magnus Therning 2009
-- License    : BSD
-- Maintainer : Magnus Therning <magnus@therning.org>

module Data.ByteString.Iteratee.Internals
    where

import Data.ByteString as BS
import Control.Monad.Trans
-- import Control.Monad.IO.Class

-- | A stream is a sequence of elements, presented in @Chunk@s.  The stream,
-- very predictably, ends with @Eof@.
data Stream
    = Eof
    | Chunk ByteString

-- | A stepper is the result type of an iteratee as it's folded over a 'Stream'.
data Stepper m a
    = Done a Stream
    | NeedAnotherChunk (Iteratee m a)

-- | An iteratee is the thing that's folded over a 'Stream'.  All iteratees are
-- expected to move to the 'Done' state when they receive 'Eof'.
newtype Iteratee m a = Iteratee
    { runIteratee :: Stream -> m (Stepper m a) }

-- | An enumerator is an iteratee transformer.  It passes a sequence of
-- 'Stream' to the iteratee, ending with 'Eof'. It stops when the iteratee
-- reaches the 'Done' state.
type Enumerator m a = Iteratee m a -> m (Iteratee m a)

-- | An enumeratee is both an iteratee and an enumerator.  It accepts a
-- sequence of 'Stream', modifies the data, and then passes it on to the inner
-- iteratee.
type Enumeratee m a = Iteratee m a -> Iteratee m (Iteratee m a)

instance (Monad m) => Monad (Iteratee m) where
    return x = Iteratee $ \ s -> return $ Done x s
    i >>= f = Iteratee $ \ s -> do
        step <- runIteratee i s
        case step of
            Done v s' -> runIteratee (f v) s'
            NeedAnotherChunk i' -> return . NeedAnotherChunk $ i' >>= f

instance (MonadIO m) => MonadIO (Iteratee m) where
    liftIO a = Iteratee $ \ s -> do
        r <- liftIO a
        return $ Done r s

-- | The most basic of enumerators, it applies the iteratee to the terminated
-- stream.
enumEof :: (Monad m) => Enumerator m a
enumEof iter = do
    step <- runIteratee iter Eof
    case step of
        Done x _ -> return $ Iteratee $ return . Done x
        NeedAnotherChunk i -> return i

-- | An enumerator which passes the given string in a single 'Chunk'.
enumPure1Chunk :: Monad m => ByteString -> Enumerator m a
enumPure1Chunk bs iter = run bs iter
    where
        run bs' i
            | BS.null bs' = runIteratee i Eof >>= checkIfDone return
            | otherwise = runIteratee i (Chunk bs') >>= checkIfDone (run empty)

-- | An enumerator which passes the given string, chopped up into the given
-- length, in a stream of 'Chunk's.
enumPureManyChunk :: Monad m => ByteString -> Int -> Enumerator m a
enumPureManyChunk bs n iter = run bs n iter
    where
        run bs' n' i
            | BS.null bs' = runIteratee i Eof >>= checkIfDone return
            | n' > 0 = let
                    (p1, p2) = BS.splitAt n' bs'
                in runIteratee i (Chunk p1) >>= checkIfDone (run p2 n')
            | otherwise = error $ "enumPureManyChunk: called with n'=" ++ show n'

-- | Useful function for defining enumerators.  See source for 'enumPure1Chunk'
-- for example of usage.
checkIfDone :: Monad m =>
    (Iteratee m a -> m (Iteratee m a)) -- ^ Function applied to the iteratee, if next argument is 'NeedAnotherChunk'
    -> Stepper m a
    -> m (Iteratee m a)
checkIfDone _ (Done x _) = return $ return x
checkIfDone f (NeedAnotherChunk i') = f i'
