module Main
    where

import Codec.Binary.DataEncoding
import Control.Monad
import Data.Char
import Data.Word
import Numeric
import System
import System.Console.GetOpt
import System.FilePath
import System.Posix.Files

ver = "omnicode encode (oenc) 0.1\n\
    \Copyright 2007 Magnus Therning <magnus@therning.org>"

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
    Option "c" ["codec"] (ReqArg setOptCodec "CODEC") "use codec",
    Option "" ["version"] (NoArg optShowVersion) "",
    Option "h" ["help"] (NoArg optShowHelp) ""
    ]

-- {{{2 option actions
setOptCodec codec opts = case codec of
    "uu" -> return opts { optEncode = chop uu 61 . encode uu }
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
