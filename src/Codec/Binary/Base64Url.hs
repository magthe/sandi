{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Codec.Binary.Base64Url
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
module Codec.Binary.Base64Url
    ( b64u_encode_part
    , b64u_encode_final
    , b64url_decode_part
    , b64url_decode_final
    , encode
    , decode
    ) where

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe
import System.IO.Unsafe as U

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum

foreign import ccall "static b64.h b64u_enc_part"
    c_b64u_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static b64.h b64u_enc_final"
    c_b64u_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import ccall "static b64.h b64u_dec_part"
    c_b64u_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall "static b64.h b64u_dec_final"
    c_b64u_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

b64u_encode_part :: BS.ByteString -> (BS.ByteString, BS.ByteString)
b64u_encode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 3 * 4
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                c_b64u_enc_part (castPtr inBuf) (castEnum inLen)
                    outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

-- todo: there is unnecessary memory used when the bytestring passed in is of length 0
b64u_encode_final :: BS.ByteString -> Maybe BS.ByteString
b64u_encode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 4
    alloca $ \ pOutLen -> do
        r <- c_b64u_enc_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r /= 0
            then return Nothing
            else do
                outLen <- peek pOutLen
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return $ Just outBs

-- todo: too much memory is used when there's an error
b64url_decode_part :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
b64url_decode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 4 * 3
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                r <- c_b64u_dec_part (castPtr inBuf) (castEnum inLen)
                    outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                if r == 0
                    then return $ Right (outBs, remBs)
                    else return $ Left (outBs, remBs)

-- todo: too much memory is used when 0 or 1 byte is encoded
b64url_decode_final :: BS.ByteString -> Maybe BS.ByteString
b64url_decode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 2
    alloca $ \ pOutLen -> do
        r <- c_b64u_dec_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r /= 0
            then return Nothing
            else do
                outLen <- peek pOutLen
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return $ Just outBs

encode :: BS.ByteString -> BS.ByteString
encode bs = let
        (first, rest) = b64u_encode_part bs
        Just final = b64u_encode_final rest
    in first `BS.append` final

decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode bs = either
    Left
    (\ (first, rest) ->
        maybe
            (Left (first, rest))
            (\ fin -> Right (first `BS.append` fin))
            (b64url_decode_final rest))
    (b64url_decode_part bs)
