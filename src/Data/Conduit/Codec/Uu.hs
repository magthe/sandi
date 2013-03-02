module Data.Conduit.Codec.Uu where

import qualified Codec.Binary.Uu as Uu
import qualified Data.Conduit.Codec.Util as U

import Data.Conduit (Conduit, MonadThrow)
import Data.ByteString (ByteString, empty)

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeI Uu.uu_encode_part Uu.uu_encode_final empty

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeI Uu.uu_decode_part Uu.uu_decode_final empty
