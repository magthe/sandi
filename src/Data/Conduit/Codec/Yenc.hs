module Data.Conduit.Codec.Yenc where

import qualified Codec.Binary.Yenc as Y
import qualified Data.Conduit.Codec.Util as U

import Data.Conduit (Conduit, MonadThrow)
import Data.ByteString (ByteString, empty)

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeII Y.encode

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeII Y.y_dec empty
