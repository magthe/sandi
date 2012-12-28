{-# LANGUAGE DeriveDataTypeable #-}

{-
oenc - command line utility for data encoding
Copyright (C) 2012  Magnus Therning

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main where

import Paths_omnicodec (version)

import Control.Exception
import Control.Monad
import Data.ByteString as BS
import Data.Maybe
import Data.Version(showVersion)
import System.Console.CmdArgs
import System.IO

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
ver = "omnicode encode (oenc) " ++ (showVersion version)
    ++ "\nCopyright 2012 Magnus Therning <magnus@therning.org>"

data Codec = B64 | B64U | B32 | B32H | B16 | B85 | QP | Uu | Xx
    deriving(Show, Eq, Data, Typeable)

codecMap :: [(Codec, BS.ByteString -> BS.ByteString)]
codecMap =
    [ (B64, B64.encode)
    , (B64U, B64U.encode)
    , (B32, B32.encode)
    , (B32H, B32H.encode)
    , (B16, B16.encode)
    , (B85, B85.encode)
    , (QP, QP.encode)
    , (Uu, Uu.encode)
    , (Xx, Xx.encode)
    -- , (PS, PS.encodeInc)
    -- , (Url, Url.encodeInc)
    ]

data MyArgs = MyArgs { argInput :: Maybe FilePath, argOutput :: Maybe FilePath, argCodec :: Codec }
    deriving(Show, Data, Typeable)

myArgs :: MyArgs
myArgs = MyArgs
    { argInput = Nothing &= name "i" &= name "in" &= explicit &= typFile &= help "read data from file"
    , argOutput = Nothing &= name "o" &= name "out" &= explicit &= typFile &= help "write encoded data to file"
    , argCodec = B64 &= name "c" &= name "codec" &= explicit &= typ "CODEC" &= help "codec b64, b64u, b32, b32h, b16, b85, qp, uu, xx (b64)"
    } &= summary ver &= details
        [ "Encoder tool for multiple encodings:"
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
            encodeFile (fromJust encFunc) inputFile outputFile

withMaybeFile :: Maybe FilePath -> IOMode -> (Handle -> IO r) -> IO r
withMaybeFile fn mode func = let
        dH = if mode == ReadMode
            then stdin
            else stdout
    in bracket 
        (maybe (return dH) (flip openFile mode) fn)
        hClose
        func

encodeFile :: (BS.ByteString -> BS.ByteString) -> Handle -> Handle -> IO ()
encodeFile encFunc inF outF = do
    inData <- BS.hGet inF 4096
    unless (BS.null inData) $ BS.hPut outF (encFunc inData) >> encodeFile encFunc inF outF
