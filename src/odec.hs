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
    where

import Codec.Binary.DataEncoding
import Data.Char
import Data.Maybe
import Data.Word
import System
import System.Console.GetOpt
import Control.Exception as CE
import System.Directory

ver = "omnicode decode (odec) 0.1\n\
    \Copyright 2007 Magnus Therning <magnus@therning.org>"

-- {{{1 Options
data DecOptions = DecOptions {
    optDecode :: [String] -> [Maybe Word8],
    optRead :: IO String,
    optWrite :: String -> IO (),
    fn :: Maybe String  -- needed in case we need to clean up later on
    }

defaultOptions = DecOptions {
    optDecode = decode' uu . unchop uu,
    optRead = getContents,
    optWrite = putStr,
    fn = Nothing
    }

-- {{{2 Command line
options :: [OptDescr (DecOptions -> IO DecOptions)]
options = [
    Option "o" ["output"] (ReqArg setOptOutput "FILE") "output to file",
    Option "c" ["codec"] (ReqArg setOptCodec "CODEC") "use codec (uu,b85,b64,b64u,b32,b32h,b16)",
    Option "" ["version"] (NoArg optShowVersion) "",
    Option "h" ["help"] (NoArg optShowHelp) ""
    ]

-- {{{2 Processing command line
setOptOutput fn opts = return opts { optWrite = writeFile fn, fn = Just fn }
setOptCodec codec opts = case codec of
    "uu" -> return opts { optDecode = decode' uu . unchop uu }
    "b85" -> return opts { optDecode = decode' base85 . unchop base85 }
    "b64" -> return opts { optDecode = decode' base64 . unchop base64 }
    "b64u" -> return opts { optDecode = decode' base64Url . unchop base64Url }
    "b32" -> return opts { optDecode = decode' base32 . unchop base32 }
    "b32h" -> return opts { optDecode = decode' base32Hex . unchop base32Hex }
    "b16" -> return opts { optDecode = decode' base16 . unchop base16 }

optShowVersion _ = putStrLn ver >> exitWith ExitSuccess
optShowHelp _ = putStrLn (usageInfo "Usage:" options) >> exitWith ExitSuccess

processFileName :: [String] -> IO DecOptions
processFileName (fn:_) = return defaultOptions { optRead = readFile fn }
processFileName _ = return defaultOptions

-- {{{1 _decode
_decode :: DecOptions -> String -> IO String
_decode opts = return .  map (chr . fromIntegral . fromMaybe (error "Illegal character")) . optDecode opts . lines

-- {{{1 main
main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOpts, msgs) = getOpt RequireOrder options args
    opts <- foldl (>>=) (processFileName nonOpts) actions
    CE.catch (optRead opts >>= _decode opts >>= optWrite opts)
        (\ (CE.SomeException e) -> maybe (return ()) removeFile (fn opts) >> throwIO e)
