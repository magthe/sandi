-- |
-- Module: Data.Conduit.Codec.Yenc
-- Copyright: (c) 2014 Magnus Therning
-- License: BSD3
module Data.Conduit.Codec.Yenc where

import qualified Codec.Binary.Yenc as Y
import qualified Data.Conduit.Codec.Util as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (Conduit)

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeII Y.encode

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeII Y.yDecode empty
