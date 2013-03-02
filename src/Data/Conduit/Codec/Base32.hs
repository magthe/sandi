module Data.Conduit.Codec.Base32 where

import qualified Codec.Binary.Base32 as B32
import qualified Data.Conduit.Codec.Util as U

import Data.Conduit (Conduit, MonadThrow)
import Data.ByteString (ByteString, empty)

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeI B32.b32_encode_part B32.b32_encode_final empty

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeI B32.b32_decode_part B32.b32_decode_final empty
