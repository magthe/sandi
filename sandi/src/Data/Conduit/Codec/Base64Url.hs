{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module: Data.Conduit.Codec.Base64Url
Copyright: (c) 2014 Magnus Therning
License: BSD3
-}
module Data.Conduit.Codec.Base64Url where

import Codec.Binary.Base64Url qualified as B64U
import Data.Conduit.Codec.Util qualified as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (ConduitT)

encode :: (Monad m) => ConduitT ByteString ByteString m ()
encode = U.encodeI B64U.b64uEncodePart B64U.b64uEncodeFinal empty

decode :: (Monad m, MonadThrow m) => ConduitT ByteString ByteString m ()
decode = U.decodeI B64U.b64uDecodePart B64U.b64uDecodeFinal empty
