{-# LANGUAGE DeriveDataTypeable #-}

{-
odec - command line utility for data decoding

Copyright (C) 2012  Magnus Therning

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License.  You may obtain a copy of the
License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the License for the
specific language governing permissions and limitations under the License.
-}

module Main where

import Paths_omnicodec (version)

import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Combinators (sinkHandle, sourceHandle)
import Data.Maybe
import Data.Version(showVersion)
import System.Console.CmdArgs
import System.IO as SIO

import qualified Data.Conduit.Codec.Base16 as B16
import qualified Data.Conduit.Codec.Base32 as B32
import qualified Data.Conduit.Codec.Base32Hex as B32H
import qualified Data.Conduit.Codec.Base64 as B64
import qualified Data.Conduit.Codec.Base64Url as B64U
import qualified Data.Conduit.Codec.Base85 as B85
import qualified Data.Conduit.Codec.QuotedPrintable as QP
import qualified Data.Conduit.Codec.Uu as Uu
import qualified Data.Conduit.Codec.Xx as Xx

ver :: String
ver = "omnicode decode (odec) " ++ showVersion version
    ++ "\nCopyright 2012 Magnus Therning <magnus@therning.org>"

data Codec = B64 | B64U | B32 | B32H | B16 | B85 | QP | Uu | Xx
    deriving(Show, Eq, Data, Typeable)

codecMap :: [(Codec, ConduitT ByteString ByteString IO ())]
codecMap =
    [ (B64, B64.decode)
    , (B64U, B64U.decode)
    , (B32, B32.decode)
    , (B32H, B32H.decode)
    , (B16, B16.decode)
    , (B85, B85.decode)
    , (QP, QP.decode)
    , (Uu, Uu.decode)
    , (Xx, Xx.decode)
    -- , (PS, PS.decode)
    -- , (Url, Url.decode)
    ]

data MyArgs = MyArgs { argInput :: Maybe FilePath, argOutput :: Maybe FilePath, argCodec :: Codec }
    deriving(Show, Data, Typeable)

myArgs :: MyArgs
myArgs = MyArgs
    { argInput = Nothing &= name "i" &= name "in" &= explicit &= typFile &= help "read encoded data from file"
    , argOutput = Nothing &= name "o" &= name "out" &= explicit &= typFile &= help "write decoded data to file"
    , argCodec = B64 &= name "c" &= name "codec" &= explicit &= typ "CODEC" &= help "codec b64, b64u, b32, b32h, b16, b85, ps, qp, url, uu, xx (b64)"
    } &= summary ver &= details
        [ "Decoder tool for multiple encodings:"
        , " b64  - base64 (default)"
        , " b64u - base64url"
        , " b32  - base32"
        , " b32h - base32hex"
        , " b16  - base16"
        , " b85  - base85"
        , " qp   - quoted printable"
        , " uu   - uu encoding"
        , " xx   - xx encoding"
        -- , " ps   - python string escaping"
        -- , " url  - url encoding"
        ]

main :: IO ()
main =
    cmdArgs myArgs >>= \ a -> do
      let decFunc = lookup (argCodec a) codecMap
      withMaybeFile (argInput a) ReadMode $ \ inputFile ->
        withMaybeFile (argOutput a) WriteMode $ \ outputFile ->
        runConduit $ decodeFile (fromJust decFunc) inputFile outputFile

withMaybeFile :: Maybe FilePath -> IOMode -> (Handle -> IO r) -> IO r
withMaybeFile fn mode func = let
        dH = if mode == ReadMode
            then stdin
            else stdout
    in bracket
        (maybe (return dH) (`openFile` mode) fn)
        hClose
        func

decodeFile :: MonadIO m => ConduitM ByteString ByteString m () -> Handle -> Handle -> ConduitM a c m ()
decodeFile decFunc inF outF = sourceHandle inF .| decFunc .| sinkHandle outF
