{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Codec.Binary.Xx
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
--
-- Xxencoding is obsolete but still included for completeness.  Further
-- information on the encoding can be found at
-- <http://en.wikipedia.org/wiki/Xxencode>.  It should be noted that this
-- implementation performs no padding.
--
-- This encoding is very similar to uuencoding, therefore further information
-- regarding the functions can be found in the documentation of
-- "Codec.Binary.Uu".
module Codec.Binary.Xx
    ( xx_encode_part
    , xx_encode_final
    , xx_decode_part
    , xx_decode_final
    , encode
    , decode
    ) where

import Data.ByteString.Unsafe
import Foreign
import Foreign.C.Types
import System.IO.Unsafe as U
import qualified Data.ByteString as BS

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum

foreign import ccall "static uu.h xx_enc_part"
    c_xx_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static uu.h xx_enc_final"
    c_xx_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import ccall "static uu.h xx_dec_part"
    c_xx_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall "static uu.h xx_dec_final"
    c_xx_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

-- | Encoding function.
--
-- >>> xx_encode_part $ Data.ByteString.Char8.pack "foo"
-- ("Naxj","")
-- >>> xx_encode_part $ Data.ByteString.Char8.pack "foob"
-- ("Naxj","b")
xx_encode_part :: BS.ByteString -> (BS.ByteString, BS.ByteString)
xx_encode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 3 * 4
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                c_xx_enc_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

-- | Encoding function for the final block.
--
-- >>> xx_encode_final $ Data.ByteString.Char8.pack "r"
-- Just "QU"
-- >>> xx_encode_final $ Data.ByteString.Char8.pack "foo"
-- Nothing
xx_encode_final :: BS.ByteString -> Maybe BS.ByteString
xx_encode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 4
    alloca $ \ pOutLen -> do
        r <- c_xx_enc_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

-- | Decoding function.
--
-- >>> xx_decode_part $ Data.ByteString.Char8.pack "Naxj"
-- Right ("foo","")
-- >>> xx_decode_part $ Data.ByteString.Char8.pack "NaxjMa3"
-- Right ("foo","Ma3")
--
-- >>> xx_decode_part $ Data.ByteString.Char8.pack "Na j"
-- Left ("","Na J")
xx_decode_part :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
xx_decode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 4 * 3
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                r <- c_xx_dec_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                if r == 0
                    then return $ Right (outBs, remBs)
                    else return $ Left (outBs, remBs)

-- | Decoding function for the final block.
--
-- >>> xx_decode_final $ Data.ByteString.Char8.pack "Naw"
-- Just "fo"
-- >>> xx_decode_final $ Data.ByteString.Char8.pack ""
-- Just ""
-- >>> xx_decode_final $ Data.ByteString.Char8.pack "Na "
-- Nothing
--
-- >>> xx_decode_final $ encode $ Data.ByteString.Char8.pack "foo"
-- Nothing
xx_decode_final :: BS.ByteString -> Maybe BS.ByteString
xx_decode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 3
    alloca $ \ pOutLen -> do
        r <- c_xx_dec_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

encode :: BS.ByteString -> BS.ByteString
encode bs = let
        (first, rest) = xx_encode_part bs
        Just final = xx_encode_final rest
    in first `BS.append` final

decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode bs = either
    Left
    (\ (first, rest) ->
        maybe
            (Left (first, rest))
            (\ fin -> Right (first `BS.append` fin))
            (xx_decode_final rest))
    (xx_decode_part bs)
