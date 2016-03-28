-- |
-- Module: Data.Conduit.Codec.Base85
-- Copyright: (c) 2014 Magnus Therning
-- License: BSD3
module Data.Conduit.Codec.Base85 where

import qualified Codec.Binary.Base85 as B85
import qualified Data.Conduit.Codec.Util as U

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString, empty)
import Data.Conduit (Conduit)

encode :: (Monad m) => Conduit ByteString m ByteString
encode = U.encodeI B85.b85EncodePart B85.b85EncodeFinal empty

decode :: (Monad m, MonadThrow m) => Conduit ByteString m ByteString
decode = U.decodeI B85.b85DecodePart B85.b85DecodeFinal empty
