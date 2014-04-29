-- |
-- Module: Data.Conduit.Codec.Xx
-- Copyright: (c) 2014 Magnus Therning
-- License: BSD3
module Data.Conduit.Codec.Xx where

import qualified Codec.Binary.Xx as Xx
import qualified Data.Conduit.Codec.Util as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (Conduit)

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeI Xx.xx_encode_part Xx.xx_encode_final empty

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeI Xx.xx_decode_part Xx.xx_decode_final empty
