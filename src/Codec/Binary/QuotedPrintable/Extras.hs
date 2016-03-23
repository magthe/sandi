-- |
-- Module: Codec.Binary.QuotedPrintable.Extras
-- Copyright: (c) 2015 Magnus Therning
-- License: BSD3
module Codec.Binary.QuotedPrintable.Extras
       (decodeWithSLB
       ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Search as BSS
import           Data.Either

import           Codec.Binary.QuotedPrintable

-- | A very simplistic implementation of decoding with soft line breaks present.
--
-- It's simplistic because it doesn't deal well with errors.
decodeWithSLB :: BS.ByteString -> BS.ByteString
decodeWithSLB = BS.concat . rights . map decode . BSS.split (BC.pack "=\r\n")
