{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module: Data.Conduit.Codec.Base16
Copyright: (c) 2014 Magnus Therning
License: BSD3
-}
module Data.Conduit.Codec.Base16 where

import Codec.Binary.Base16 qualified as B16
import Data.Conduit.Codec.Util qualified as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (ConduitT)

encode :: (Monad m) => ConduitT ByteString ByteString m ()
encode = U.encodeII B16.encode

decode :: (Monad m, MonadThrow m) => ConduitT ByteString ByteString m ()
decode = U.decodeII B16.b16Dec empty
