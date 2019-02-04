-- |
-- Module: Data.Conduit.Codec.Xx
-- Copyright: (c) 2014 Magnus Therning
-- License: BSD3
module Data.Conduit.Codec.Xx where

import qualified Codec.Binary.Xx as Xx
import qualified Data.Conduit.Codec.Util as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (ConduitT)

encode :: (Monad m) => ConduitT ByteString ByteString m ()
encode = U.encodeI Xx.xxEncodePart Xx.xxEncodeFinal empty

decode :: (Monad m, MonadThrow m) => ConduitT ByteString ByteString m ()
decode = U.decodeI Xx.xxDecodePart Xx.xxDecodeFinal empty
