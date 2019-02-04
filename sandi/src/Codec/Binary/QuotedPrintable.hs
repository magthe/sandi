{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Codec.Binary.QuotedPrintable
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
--
-- Implementation of Quoted-Printable based on RFC 2045
-- (<http://tools.ietf.org/html/rfc2045>).
module Codec.Binary.QuotedPrintable
    ( qpEncode
    , qpEncodeSL
    , qpDecode
    , encode
    , decode
    ) where

import Data.List
import Foreign
import Foreign.C.Types
import System.IO.Unsafe as U
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum

foreign import ccall "static qp.h qp_enc"
    c_qp_enc :: Word8 -> Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static qp.h qp_dec"
    c_qp_dec :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

-- | Encoding function.
--
-- This function encodes /everything/ that is passed in, it will not try to
-- guess the native line ending for your architecture.  In other words, if you
-- are using this to encode text you need to split it into separate lines
-- before encoding.
--
-- This function allocates enough space to hold twice the size of the indata
-- (or at least 512 bytes) and then encodes as much as possible of the indata.
-- That means there is a risk that the encoded data won't fit and in that case
-- the second part of the pair contains the remainder of the indata.
--
-- >>> qpEncode $ Data.ByteString.Char8.pack "="
-- ("=3D","")
-- >>> snd $ qpEncode $ Data.ByteString.Char8.pack $ Data.List.take 171 $ repeat '='
-- "="
--
-- All space (0x20) and tab (0x9) characters are encoded:
--
-- >>> qpEncode $ Data.ByteString.Char8.pack " \t"
-- ("=20=09","")
--
-- Since the input is supposed to have been split prior to calling this
-- function all occurances of CR and LF are encoded.
--
-- >>> qpEncode $ Data.ByteString.Char8.pack "\n\r\r\n\n\r"
-- ("=0A=0D=0D=0A=0A=0D","")
--
-- Soft line breaks are inserted as needed
--
-- >>> qpEncode $ Data.ByteString.Char8.pack "========================="
-- ("=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=\r\n=3D","")
qpEncode :: BS.ByteString -> (BS.ByteString, BS.ByteString)
qpEncode = qpEnc' 1

-- | Single line encoding function.
--
-- Like 'qpEncode', but without inserting soft line breaks.
qpEncodeSL :: BS.ByteString -> (BS.ByteString, BS.ByteString)
qpEncodeSL = qpEnc' 0

qpEnc' :: Word8 -> BS.ByteString -> (BS.ByteString, BS.ByteString)
qpEnc' split bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutBuf = max 512 (2 * inLen)
    outBuf <- mallocBytes maxOutBuf
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutBuf)
                c_qp_enc split (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- BSU.unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return (outBs, remBs)

-- | Decoding function.
--
-- >>> qpDecode $ Data.ByteString.Char8.pack "foobar"
-- Right "foobar"
-- >>> qpDecode $ Data.ByteString.Char8.pack "1=20+=201=20=3D=202"
-- Right "1 + 1 = 2"
--
-- The input data is allowed to use lowercase letters in the hexadecimal
-- representation of an octets value, even though the standard says that only
-- uppercase letters may be used:
--
-- >>> qpDecode $ Data.ByteString.Char8.pack "=3D"
-- Right "="
-- >>> qpDecode $ Data.ByteString.Char8.pack "=3d"
-- Right "="
--
-- It also allows the input to encode _all_ octets in the hexadecimal
-- representation:
--
-- >>> qpDecode $ Data.ByteString.Char8.pack "=20!"
-- Right (" !","")
-- >>> qpDecode $ Data.ByteString.Char8.pack "=20=21"
-- Right (" !","")
--
-- A @Left@ value is only ever returned on decoding errors.
--
-- >>> qpDecode $ Data.ByteString.Char8.pack "=2"
-- Right ("","=2")
-- >>> qpDecode $ Data.ByteString.Char8.pack "=2g"
-- Left ("","=2g")
--
-- Per the specification a CRLF pair is left in, but a single CR or LF is an
-- error.
--
-- >>> qpDecode $ Data.ByteString.Char8.pack "\r\n"
-- Right ("\r\n","")
-- >>> qpDecode $ Data.ByteString.Char8.pack "\n"
-- Left ("","\n")
-- >>> qpDecode $ Data.ByteString.Char8.pack "\r"
-- Left ("","\r")
--
-- the same goes for space and tab characters
--
-- >>> qpDecode $ Data.ByteString.Char8.pack " \t"
-- Right (" \t","")
--
-- The function deals properly with soft line breaks.
--
-- >>> qpDecode $ Data.ByteString.Char8.pack " =\r\n"
-- Right (" ","")
qpDecode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
qpDecode bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes inLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum inLen)
                r <- c_qp_dec (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- BSU.unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                if r == 0
                    then return $ Right (outBs, remBs)
                    else return $ Left (outBs, remBs)

-- | Convenient function that calls 'qpEncode' repeatedly until the whole input
-- data is encoded.
encode :: BS.ByteString -> BS.ByteString
encode = BS.concat . takeWhile (not . BS.null) . unfoldr (Just . qpEncode)

-- | A synonym for 'qpDec'.
decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode = either Left goR . qpDecode
  where
    goR a@(d, r) = if BS.null r then Right d else Left a
