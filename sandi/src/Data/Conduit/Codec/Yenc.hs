{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module: Data.Conduit.Codec.Yenc
Copyright: (c) 2014 Magnus Therning
License: BSD3
-}
module Data.Conduit.Codec.Yenc where

import Codec.Binary.Yenc qualified as Y
import Data.Conduit.Codec.Util qualified as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (ConduitT)

encode :: (Monad m) => ConduitT ByteString ByteString m ()
encode = U.encodeII Y.encode

decode :: (Monad m, MonadThrow m) => ConduitT ByteString ByteString m ()
decode = U.decodeII Y.yDecode empty
