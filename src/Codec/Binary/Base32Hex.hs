{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module    : Codec.Binary.Base32Hex
-- Copyright : (c) 2012 Magnus Therning
-- License   : BSD3
module Codec.Binary.Base32Hex
   ( b32h_encode_part
   , b32h_encode_final
   , b32h_decode_part
   , b32h_decode_final
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

foreign import ccall "static b32.h b32h_enc_part"
    c_b32h_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static b32.h b32h_enc_final"
    c_b32h_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import ccall "static b32.h b32h_dec_part"
    c_b32h_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall "static b32.h b32h_dec_final"
    c_b32h_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

b32h_encode_part :: BS.ByteString -> (BS.ByteString, BS.ByteString)
b32h_encode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 5 * 8
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                c_b32h_enc_part (castPtr inBuf) (castEnum inLen)
                    outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

-- todo: there is unnecessary memory used when the bytestring passed in is of length 0
b32h_encode_final :: BS.ByteString -> Maybe BS.ByteString
b32h_encode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 4
    alloca $ \ pOutLen -> do
        r <- c_b32h_enc_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r /= 0
            then return Nothing
            else do
                outLen <- peek pOutLen
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return $ Just outBs

-- todo: too much memory is used when there's an error
b32h_decode_part :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
b32h_decode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 8 * 5
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                r <- c_b32h_dec_part (castPtr inBuf) (castEnum inLen)
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
b32h_decode_final :: BS.ByteString -> Maybe BS.ByteString
b32h_decode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 2
    alloca $ \ pOutLen -> do
        r <- c_b32h_dec_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r /= 0
            then return Nothing
            else do
                outLen <- peek pOutLen
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return $ Just outBs

encode :: BS.ByteString -> BS.ByteString
encode bs = let
        (first, rest) = b32h_encode_part bs
        Just final = b32h_encode_final rest
    in first `BS.append` final

decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode bs = either
    Left
    (\ (first, rest) ->
        maybe
            (Left (first, rest))
            (\ fin -> Right (first `BS.append` fin))
            (b32h_decode_final rest))
    (b32h_decode_part bs)
