-- |
-- Module     : Data.ByteString.Iteratee
-- Copyright  : (c) Magnus Therning 2009
-- License    : BSD
-- Maintainer : Magnus Therning <magnus@therning.org>
-- 
-- An implementation of iteratee-based IO for @ByteString@s only, inspired by
-- code based on Oleg Kiselyov's paper <http://okmij.org/ftp/Haskell/Iteratee/>.

module Data.ByteString.Iteratee
    (
    -- * Types
      Iteratee
    , Enumerator
    , Enumeratee

    -- * Enumerators
    , enumHandle

    -- * Iteratees
    -- ** Iteratee utilities
    , run

    -- ** Basic iteratees
    , identity
    , Data.ByteString.Iteratee.head
    , heads
    , peek
    , Data.ByteString.Iteratee.dropWhile
    , Data.ByteString.Iteratee.takeWhile
    , Data.ByteString.Iteratee.drop
    , Data.ByteString.Iteratee.take

    -- ** Misc iteratees
    , applyToN
    , takeUntilDelim

    -- ** Sinks
    , sinkHandle
    , sinkFile

    -- * Enumeratees
    , applyFun
    ) where

import qualified Debug.Trace as DT

import Data.ByteString as BS
import Data.Word
import System.IO
import Control.Exception as CE
import System.IO.Error
import Data.ByteString.Iteratee.Internals as DBII
import Control.Monad.Trans
import System.IO

run :: Monad m => Iteratee m a -> m a
run iter = do
    step <- runIteratee iter Eof
    case step of
        Done a _ -> return a
        NeedAnotherChunk _ -> error "run: Iterator didn't finish on Eof"

-- | An enumerator of a file handle.  It uses 'hGetNonBlocking' in order to
-- pass in available data as quickly as possible.  Seeking is not supported.
enumHandle :: MonadIO m => Handle -> Enumerator m a
enumHandle h iter = let
        tryRead i = do
            res <- liftIO $ catchEof $ BS.hGetNonBlocking h 1024
            case res of
                Nothing -> doStepEof i
                Just d -> if BS.null d
                    then waitAndRetry i
                    else doStep i d

        doStepEof i = runIteratee i Eof >>= checkIfDone return
        doStep i d = runIteratee i (Chunk d) >>= checkIfDone tryRead

        waitAndRetry i = do
            res <- liftIO $ catchEof $ hWaitForInput h (-1)
            case res of
                Nothing -> doStepEof i
                Just _ -> tryRead i

        catchEof a = do
            res <- CE.try a
            case res of
                Left e -> if isEOFError e
                    then return Nothing
                    else throw e
                Right v -> return $ Just v

    in tryRead iter

-- | The identity iteratee, doesn't do anything.
identity :: Monad m => Iteratee m ()
identity = return ()

-- | Attempt to read the next element in the stream.
head :: Monad m => Iteratee m (Maybe Word8)
head = Iteratee step
    where
        step Eof = return $ Done Nothing Eof
        step (Chunk bs) = if BS.null bs
            then return $ NeedAnotherChunk Data.ByteString.Iteratee.head
            else return $ Done (Just $ BS.head bs) (Chunk $ BS.tail bs)

-- | Attempts to match the give sequence of elements against the elements in
-- the stream.  The matched elements are removed from the stream.  Return the
-- number of elements that were matched.
heads :: Monad m => [Word8] -> Iteratee m Int
heads l = Iteratee $ step 0 l
    where
        step a _ Eof = return $ Done a Eof
        step a [] s = return $ Done a s
        step a l@(w:ws) s@(Chunk bs) = if BS.null bs
            then return $ NeedAnotherChunk $ Iteratee $ step a l
            else if w == BS.head bs
                then step (succ a) ws (Chunk $ BS.tail bs)
                else return $ Done a s

-- | Look at the first element in the stream, without removing it.
peek :: Monad m => Iteratee m (Maybe Word8)
peek = Iteratee step
    where
        step Eof = return $ Done Nothing Eof
        step s@(Chunk bs) = if BS.null bs
            then return $ NeedAnotherChunk $ Iteratee step
            else return $ Done (Just $ BS.head bs) s

-- | Take elements off the stream while the predicate is true.
takeWhile :: Monad m => (Word8 -> Bool) -> Iteratee m ByteString
takeWhile test = Iteratee $ step empty
    where
        step a Eof = return $ Done a Eof
        step a (Chunk bs) = let
                (p1, p2) = BS.span test bs
            in if BS.null p2
                then return $ NeedAnotherChunk $ Iteratee $ step (a `append` p1)
                else return $ Done (a `append` p1) (Chunk p2)

-- | Skip elements in the stream while the predicate is true.
dropWhile :: Monad m => (Word8 -> Bool) -> Iteratee m ()
dropWhile test = Iteratee step
    where
        step Eof = return $ Done () Eof
        step (Chunk bs) = let
                (p1,p2) = BS.span test bs
            in if BS.null p2
                then return $ NeedAnotherChunk $ Data.ByteString.Iteratee.dropWhile test
                else return $ Done () (Chunk p2)

-- | Take @n@ elements off the stream.
take :: Monad m => Int -> Iteratee m ByteString
take n = Iteratee $ step n empty
    where
        step 0 a s = return $ Done a s
        step _ a Eof = return $ Done a Eof
        step i a (Chunk bs) = let
                (p1, p2) = BS.splitAt i bs
                l1 = BS.length p1
            in if BS.null p2
                then return $ NeedAnotherChunk $ Iteratee $ step (i - l1) (a `append` p1)
                else return $ Done (a `append` p1) (Chunk p2)

-- | Skip @n@ elements in the stream.
drop :: Monad m => Int -> Iteratee m ()
drop n = Iteratee $ step n
    where
        step 0 s = return $ Done () s
        step i Eof = return $ Done () Eof
        step i (Chunk bs) = let
                (p1, p2) = BS.splitAt i bs
                l1 = BS.length p1
            in if BS.null p2
                then return $ NeedAnotherChunk $ Iteratee $ step (i - l1)
                else return $ Done () (Chunk p2)

-- | Apply the given iteratee to a chunk of at most length @n@.  Returns @Just
-- a@ on success, and @Nothing@ otherwise (i.e. if the iteratee required more
-- than @n@ elements).  Only on success will the @n@ elements be consumed.
--
-- This iteratee is useful to implement restrictions on length, e.g. that the
-- first line must be at most 250 bytes.
--
-- /N.B./ This iteratee might "eat" an 'EOF', it's important to make sure that
-- the stream still is properly terminated.
applyToN :: Monad m => Int -> Iteratee m a -> Iteratee m (Maybe a)
applyToN n iter = let
        wrap i = Iteratee $ \ s -> do
            step <- runIteratee i s
            case step of
                NeedAnotherChunk i' -> return . NeedAnotherChunk $ wrap i'
                Done v r -> do
                    step' <- runIteratee iter (Chunk v)
                    case step' of
                        Done v' r' ->
                            return $ Done (Just v') (combineChunks r' r)
                        NeedAnotherChunk _ ->
                            return $ Done Nothing (combineChunks (Chunk v) r)

        -- partial definition only, relying on the usage above, "r" above is
        -- always the second argument, and only it can ever be Eof
        combineChunks c@(Chunk bs) Eof
            | BS.null bs = Eof -- local take will return empty on EOF, so keep it
            | otherwise = c
        combineChunks (Chunk lbs) (Chunk rbs) = Chunk $ lbs `append` rbs

    in wrap (Data.ByteString.Iteratee.take n)

-- | Take elements until the given delimiter is encountered.  The characters of
-- the delimiter are discarded.
takeUntilDelim :: Monad m => [Word8] -> Iteratee m ByteString
takeUntilDelim d = let
        h = Prelude.head d
        l = Prelude.length d

        step1 a = do
            r <- Data.ByteString.Iteratee.takeWhile (/= h)
            m <- heads d
            step2 (a `append` r) m

        step2 a m
            | m == 0 = return a
            | m < l = step1 (a `append` (pack $ Prelude.take m d))
            | otherwise = return a

    in step1 empty

sinkHandle :: MonadIO m => Handle -> Iteratee m ()
sinkHandle h = Iteratee step
    where
        step Eof = return $ Done () Eof
        step (Chunk bs) = do
            liftIO $ hPut h bs
            return $ NeedAnotherChunk $ Iteratee step

sinkFile :: MonadIO m => FilePath -> Iteratee m ()
sinkFile fp = do
    h <- liftIO $ openBinaryFile fp ReadMode
    sinkHandle h
    liftIO $ hClose h

-- | An enumeratee which applies a function to each chunk before passing it on.
applyFun :: Monad m => (ByteString -> ByteString ) -> Enumeratee m a
applyFun f i = Iteratee $ step i
    where
        step iter Eof = do
            ir <- runIteratee iter Eof
            case ir of
                Done a _ -> return $ Done (Iteratee $ \ _ -> return $ Done a Eof) Eof

        step iter c@(Chunk bs) = let
                nbs = f bs
            in do
                ir <- runIteratee iter (Chunk nbs)
                case ir of
                    Done a _ -> return $ Done (Iteratee $ \ _ -> return $ Done a (Chunk BS.empty)) (Chunk BS.empty)
                    NeedAnotherChunk niter -> return  $ NeedAnotherChunk $ Iteratee (step niter)
