-- |
-- Module: Data.Conduit.Codec.QuotedPrintable
-- Copyright: (c) 2014 Magnus Therning
-- License: BSD3
module Data.Conduit.Codec.QuotedPrintable where

import qualified Codec.Binary.QuotedPrintable as Qp
import qualified Data.Conduit.Codec.Util as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (ConduitT)

encode :: (Monad m) => ConduitT ByteString ByteString m ()
encode = U.encodeII Qp.encode

decode :: (Monad m, MonadThrow m) => ConduitT ByteString ByteString m ()
decode = U.decodeII Qp.qpDecode empty
