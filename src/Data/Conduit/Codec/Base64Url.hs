module Data.Conduit.Codec.Base64Url where

import qualified Codec.Binary.Base64Url as B64U
import qualified Data.Conduit.Codec.Util as U

import Data.Conduit (Conduit, MonadThrow)
import Data.ByteString (ByteString, empty)

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeI B64U.b64u_encode_part B64U.b64u_encode_final empty

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeI B64U.b64u_decode_part B64U.b64u_decode_final empty
