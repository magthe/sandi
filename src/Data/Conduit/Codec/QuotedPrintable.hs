module Data.Conduit.Codec.QuotedPrintable where

import qualified Codec.Binary.QuotedPrintable as Qp
import qualified Data.Conduit.Codec.Util as U

import Data.Conduit
import Data.ByteString as BS

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeII Qp.encode

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeII Qp.qp_dec empty
