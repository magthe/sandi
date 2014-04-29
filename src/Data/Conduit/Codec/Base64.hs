module Data.Conduit.Codec.Base64 where

import qualified Codec.Binary.Base64 as B64
import qualified Data.Conduit.Codec.Util as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (Conduit)

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeI B64.b64_encode_part B64.b64_encode_final empty

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeI B64.b64_decode_part B64.b64_decode_final empty
