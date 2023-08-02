{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module: Data.Conduit.Codec.Uu
Copyright: (c) 2014 Magnus Therning
License: BSD3
-}
module Data.Conduit.Codec.Uu where

import Codec.Binary.Uu qualified as Uu
import Data.Conduit.Codec.Util qualified as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (ConduitT)

encode :: (Monad m) => ConduitT ByteString ByteString m ()
encode = U.encodeI Uu.uuEncodePart Uu.uuEncodeFinal empty

decode :: (Monad m, MonadThrow m) => ConduitT ByteString ByteString m ()
decode = U.decodeI Uu.uuDecodePart Uu.uuDecodeFinal empty
