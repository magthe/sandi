{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module: Data.Conduit.Codec.Util
-- Copyright: (c) 2014 Magnus Therning
-- License: BSD3

module Data.Conduit.Codec.Util
    ( CodecDecodeException(..)
    , encodeI
    , decodeI
    , decodeII
    , encodeII
    ) where

import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Data.ByteString as BS (ByteString, append, null)
import Data.Conduit (Conduit, await, yield)
import Data.Maybe (fromJust)
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadThrow, throwM)

type EncFunc = ByteString -> ByteString
type EncFuncPart = ByteString -> (ByteString, ByteString)
type EncFuncFinal = ByteString -> Maybe ByteString
type DecFunc = ByteString -> Either (ByteString, ByteString) (ByteString, ByteString)
type DecFuncFinal = ByteString -> Maybe ByteString

data CodecDecodeException = CodecDecodeException ByteString
    deriving (Typeable, Show)

instance Exception CodecDecodeException

encodeI :: (Monad m) => EncFuncPart -> EncFuncFinal -> ByteString -> Conduit ByteString m ByteString
encodeI enc_part enc_final i = do
    clear <- await
    case clear of
        Nothing -> void (yield $ fromJust $ enc_final i)
        Just s -> let
                (a, b) = enc_part (i `append` s)
            in do
                unless (BS.null a) $ yield a
                encodeI enc_part enc_final b

decodeI :: (Monad m, MonadThrow m) => DecFunc -> DecFuncFinal -> ByteString -> Conduit ByteString m ByteString
decodeI dec_part dec_final i = do
    enc <- await
    case enc of
        Nothing ->
            case dec_final i of
                Nothing -> throwM (CodecDecodeException i)
                Just s -> void (yield s)
        Just s ->
            case dec_part (i `append` s) of
                Left (a, b) -> do
                    unless (BS.null a) $ yield a
                    throwM (CodecDecodeException b)
                Right (a, b) -> do
                    unless (BS.null a) $ yield a
                    decodeI dec_part dec_final b

encodeII :: (Monad m) => EncFunc -> Conduit ByteString m ByteString
encodeII enc = do
    clear <- await
    case clear of
        Nothing -> return ()
        Just s -> do
            yield $ enc s
            encodeII enc

decodeII :: (Monad m, MonadThrow m) => DecFunc -> ByteString -> Conduit ByteString m ByteString
decodeII dec i = do
    enc <- await
    case enc of
        Nothing -> unless (BS.null i) (throwM $ CodecDecodeException i)
        Just s -> case dec $ i `append` s of
            Left (c, b) -> do
                unless (BS.null c) $ yield c
                throwM $ CodecDecodeException b
            Right (c, r) -> do
                unless (BS.null c) $ yield c
                decodeII dec r
