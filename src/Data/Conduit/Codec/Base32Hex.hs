-- |
-- Module: Data.Conduit.Codec.Base32Hex
-- Copyright: (c) 2014 Magnus Therning
-- License: BSD3
module Data.Conduit.Codec.Base32Hex where

import qualified Codec.Binary.Base32Hex as B32H
import qualified Data.Conduit.Codec.Util as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (Conduit)

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeI B32H.b32hEncodePart B32H.b32hEncodeFinal empty

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeI B32H.b32hDecodePart B32H.b32hDecodeFinal empty
