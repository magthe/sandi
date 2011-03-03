{-# LANGUAGE DeriveDataTypeable #-}

{-
oenc - command line utility for data encoding
Copyright (C) 2008  Magnus Therning

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

import Codec.Binary.Base64 as B64
import qualified Codec.Binary.Base64Url as B64U
import qualified Codec.Binary.Base32 as B32
import qualified Codec.Binary.Base32Hex as B32H
import qualified Codec.Binary.Base16 as B16
import qualified Codec.Binary.Base85 as B85
import qualified Codec.Binary.PythonString as PS
import qualified Codec.Binary.QuotedPrintable as QP
import qualified Codec.Binary.Url as Url
import qualified Codec.Binary.Uu as Uu
import qualified Codec.Binary.Xx as Xx
import Data.ByteString.Iteratee
import Data.ByteString.Iteratee.Internals

import Data.Version(showVersion)
import System.Console.CmdArgs
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.IO
import Data.Maybe

-- {{{1 command line options
ver :: String
ver = "omnicode encode (oenc) " ++ (showVersion version)
    ++ "\nCopyright 2007-2011 Magnus Therning <magnus@therning.org>"

data Codec = B64 | B64U | B32 | B32H | B16 | B85 | PS | QP | Url | Uu | Xx
    deriving(Show, Eq, Data, Typeable)

codecMap :: [(Codec, EncIncData -> EncIncRes String)]
codecMap =
    [ (B64, B64.encodeInc)
    , (B64U, B64U.encodeInc)
    , (B32, B32.encodeInc)
    , (B32H, B32H.encodeInc)
    , (B16, B16.encodeInc)
    , (B85, B85.encodeInc)
    , (PS, PS.encodeInc)
    , (QP, QP.encodeInc)
    , (Url, Url.encodeInc)
    , (Uu, Uu.encodeInc)
    , (Xx, Xx.encodeInc)
    ]

data MyArgs = MyArgs { argInput :: Maybe FilePath, argOutput :: Maybe FilePath, argCodec :: Codec }
    deriving(Show, Data, Typeable)

myArgs :: MyArgs
myArgs = MyArgs
    { argInput = Nothing &= name "i" &= name "in" &= explicit &= typFile &= help "read data from file"
    , argOutput = Nothing &= name "o" &= name "out" &= explicit &= typFile &= help "write encoded data to file"
    , argCodec = B64 &= name "c" &= name "codec" &= explicit &= typ "CODEC" &= help "codec b64, b64u, b32, b32h, b16, b85, ps, qp, url, uu, xx (b64)"
    } &= summary ver &= details
        [ "Encoder tool for multiple encodings:"
        , " b64  - base64 (default)"
        , " b64u - base64url"
        , " b32  - base32"
        , " b32h - base32hex"
        , " b16  - base16"
        , " b85  - base85"
        , " ps   - python string escaping"
        , " qp   - quoted printable"
        , " url  - url encoding"
        , " uu   - uu encoding"
        , " xx   - xx encoding"
        ]

-- {{{1 encode enumeratee
encEnumeratee :: Monad m => (EncIncData -> EncIncRes String) -> Enumeratee m a
encEnumeratee encF iter = Iteratee $ step encF iter
    where
        step f i Eof = let
                EFinal ebs = f EDone
            in do
                ir <- runIteratee i (Chunk $ BSC.pack ebs)
                case ir of
                    Done a _ -> return $ Done (Iteratee $ \ _ -> return $ Done a Eof) Eof
                    NeedAnotherChunk i' -> do
                        ir' <- runIteratee i' Eof
                        case ir' of
                            Done a _ -> return $ Done (Iteratee $ \ _ -> return $ Done a Eof) Eof
                            NeedAnotherChunk _ -> error "encEnumeratee: inner iteratee diverges on Eof"
        step f i (Chunk bs) = let
                EPart ebs f' = f (EChunk $ BS.unpack bs)
            in do
                ir <- runIteratee i (Chunk $ BSC.pack ebs)
                case ir of
                    Done a _ -> return $ Done (Iteratee $ \ _ -> return $ Done a (Chunk BS.empty)) (Chunk BS.empty)
                    NeedAnotherChunk i' -> return $ NeedAnotherChunk $ Iteratee $ step f' i'

-- {{{1 main
main :: IO ()
main = do
    cmdArgs myArgs >>= \ a -> do
    hIn <- maybe (return stdin) (\ fn -> openFile fn ReadMode) (argInput a)
    hOut <- maybe (return stdout) (\ fn -> openFile fn WriteMode) (argOutput a)
    let eI = fromJust $ lookup (argCodec a) codecMap
    (enumHandle hIn $ encEnumeratee eI (sinkHandle hOut)) >>= run >>= run
    hClose hOut
