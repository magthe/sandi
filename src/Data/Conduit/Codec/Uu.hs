-- |
-- Module: Data.Conduit.Codec.Uu
-- Copyright: (c) 2014 Magnus Therning
-- License: BSD3
module Data.Conduit.Codec.Uu where

import qualified Codec.Binary.Uu as Uu
import qualified Data.Conduit.Codec.Util as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (Conduit)

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeI Uu.uuEncodePart Uu.uuEncodeFinal empty

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeI Uu.uuDecodePart Uu.uuDecodeFinal empty
