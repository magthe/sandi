module Data.Conduit.Codec.Base85 where

import qualified Codec.Binary.Base85 as B85
import qualified Data.Conduit.Codec.Util as U

import Data.Conduit (Conduit, MonadThrow)
import Data.ByteString (ByteString, empty)

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeI B85.b85_encode_part B85.b85_encode_final empty

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeI B85.b85_decode_part B85.b85_decode_final empty
