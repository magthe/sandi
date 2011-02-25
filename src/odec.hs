{-# LANGUAGE DeriveDataTypeable #-}

{-
odec - command line utility for data decoding
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

module Main
    ( main
    ) where

-- import qualified Codec.Binary.Base64 as Base64
-- import qualified Codec.Binary.Base32 as Base32

import Paths_omnicodec (version)

import System.Console.CmdArgs
import Data.Version
import Data.Enumerator as E
import Data.Enumerator.Binary as EB
import System.IO
import Control.Monad.IO.Class
import Data.ByteString.Lazy

-- {{{1 command line arguments
data Codec = B64 | B32
    deriving(Show, Data, Typeable)

data MyArgs = MyArgs { argOutput :: FilePath, argCodec :: Codec }
    deriving(Show, Data, Typeable)

ver :: String
ver = "omnicode decode (odec) " ++ (showVersion version)
    ++ "\nCopyright 2007-2011 Magnus Therning <magnus@therning.org>"

myArgs :: MyArgs
myArgs = MyArgs
    { argOutput = def &= name "o" &= explicit &= typFile &= help "output to file"
    , argCodec = B64 &= name "c" &= typ "CODEC" &= help "codec"
    } &= summary ver

-- {{{1 main
main :: IO ()
main = do
    cmdArgs myArgs >>= \ a -> print a
    d <- run_ $ enumHandle 2048 stdin $$ encIter
    print d

-- {{{1 iteratee
encIter = do
    d <- EB.take 8
    let d' = unpack d
    return d'
