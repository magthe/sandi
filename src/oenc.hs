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

module Main
    where

import Codec.Binary.DataEncoding
import Control.Monad
import Data.Char
import Data.Version (showVersion)
import Data.Word
import Numeric
import System
import System.Console.GetOpt
import System.FilePath

import Paths_omnicodec (version)

ver = "omnicode encode (oenc) " ++ (showVersion version)
    ++ "\nCopyright 2007-2009 Magnus Therning <magnus@therning.org>"

-- {{{1 Options
data EncOptions = EncOptions {
    optEncode :: [Word8] -> [String],
    optRead :: IO String,
    optWrite :: String -> IO ()
    }

defaultOptions = EncOptions {
    optEncode = chop uu 61 . encode uu,
    optRead = getContents,
    optWrite = putStr
    }

options :: [OptDescr (EncOptions -> IO EncOptions)]
options = [
    Option "o" ["output"] (ReqArg setOptOutput "FILE") "output to file",
    Option "c" ["codec"] (ReqArg setOptCodec "CODEC") "use codec (uu,b85,b64,b64u,b32,b32h,b16)",
    Option "" ["version"] (NoArg optShowVersion) "",
    Option "h" ["help"] (NoArg optShowHelp) ""
    ]

-- {{{2 option actions
setOptCodec codec opts = case codec of
    "uu" -> return opts { optEncode = chop uu 61 . encode uu }
    "b85" -> return opts { optEncode = chop base85 60 . encode base85 }
    "b64" -> return opts { optEncode = chop base64 60 . encode base64 }
    "b64u" -> return opts { optEncode = chop base64Url 60 . encode base64Url }
    "b32" -> return opts { optEncode = chop base32 60 . encode base32 }
    "b32h" -> return opts { optEncode = chop base32Hex 60 . encode base32Hex }
    "b16" -> return opts { optEncode = chop base16 60 . encode base16 }

setOptOutput fn opts = return opts { optWrite = writeFile fn }
optShowVersion _ = putStrLn ver >> exitWith ExitSuccess
optShowHelp _ = putStr (usageInfo "Usage:" options) >> exitWith ExitSuccess

processFileName :: [String] -> IO EncOptions
processFileName (fn:_) = return defaultOptions { optRead = readFile fn }
processFileName _ = return defaultOptions

-- {{{1 encode
encode' :: EncOptions -> String -> IO String
encode' opts = return . unlines . optEncode opts . map (fromIntegral . ord)

-- {{{1 main
main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOpts, msgs) = getOpt RequireOrder options args
    opts <- foldl (>>=) (processFileName nonOpts) actions
    optRead opts >>= encode' opts >>= optWrite opts
