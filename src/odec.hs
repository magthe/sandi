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
import Control.Monad
import Data.ByteString as BS
import Data.Maybe
import Data.Version(showVersion)
import System.Console.CmdArgs
import System.Exit
import System.IO as SIO

import Codec.Binary.Base64 as B64
import qualified Codec.Binary.Base64Url as B64U
import qualified Codec.Binary.Base32 as B32
import qualified Codec.Binary.Base32Hex as B32H
import qualified Codec.Binary.Base16 as B16
import qualified Codec.Binary.Base85 as B85
import qualified Codec.Binary.QuotedPrintable as QP
import qualified Codec.Binary.Uu as Uu
import qualified Codec.Binary.Xx as Xx
-- import qualified Codec.Binary.PythonString as PS
-- import qualified Codec.Binary.Url as Url

-- {{{1 command line options
ver :: String
ver = "omnicode decode (odec) " ++ (showVersion version)
    ++ "\nCopyright 2012 Magnus Therning <magnus@therning.org>"

data Codec = B64 | B64U | B32 | B32H | B16 | B85 | QP | Uu | Xx
    deriving(Show, Eq, Data, Typeable)

codecMap :: [(Codec, BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString)]
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

-- {{{1 main
main :: IO ()
main = do
    cmdArgs myArgs >>= \ a -> do
    let encFunc = lookup (argCodec a) codecMap
    withMaybeFile (argInput a) ReadMode $ \ inputFile ->
        withMaybeFile (argOutput a) WriteMode $ \ outputFile ->
            decodeFile (fromJust encFunc) inputFile outputFile

withMaybeFile :: Maybe FilePath -> IOMode -> (Handle -> IO r) -> IO r
withMaybeFile fn mode func = let
        dH = if mode == ReadMode
            then stdin
            else stdout
    in bracket 
        (maybe (return dH) (flip openFile mode) fn)
        hClose
        func

decodeFile :: (BS.ByteString -> Either a BS.ByteString) -> Handle -> Handle -> IO ()
decodeFile decFunc inF outF = let
        writeToFile d = either
            (const $ SIO.hPutStrLn stderr "Decoding failed!" >> exitFailure)
            (BS.hPut outF)
            (decFunc d)
    in do
        inData <- BS.hGet inF 4096
        unless (BS.null inData) $ writeToFile inData >> decodeFile decFunc inF outF
