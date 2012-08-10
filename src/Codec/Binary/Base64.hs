{-# LANGUAGE ForeignFunctionInterface #-}
module Codec.Binary.Base64
    ( b64encode_part
    , b64encode_final
    , b64decode_part
    , b64decode_final
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

foreign import ccall "static b64.h b64_enc_part"
    c_b64_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static b64.h b64_enc_final"
    c_b64_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import ccall "static b64.h b64_dec_part"
    c_b64_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall "static b64.h b64_dec_final"
    c_b64_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

b64encode_part :: BS.ByteString -> (BS.ByteString, BS.ByteString)
b64encode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 3 * 4
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                c_b64_enc_part (castPtr inBuf) (castEnum inLen)
                    outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

-- todo: there is unnecessary memory used when the bytestring passed in is of length 0
b64encode_final :: BS.ByteString -> Maybe BS.ByteString
b64encode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 4
    alloca $ \ pOutLen -> do
        r <- c_b64_enc_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r /= 0
            then return Nothing
            else do
                outLen <- peek pOutLen
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return $ Just outBs

-- todo: too much memory is used when there's an error
b64decode_part :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
b64decode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 4 * 3
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                r <- c_b64_dec_part (castPtr inBuf) (castEnum inLen)
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
b64decode_final :: BS.ByteString -> Maybe BS.ByteString
b64decode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 2
    alloca $ \ pOutLen -> do
        r <- c_b64_dec_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r /= 0
            then return Nothing
            else do
                outLen <- peek pOutLen
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return $ Just outBs

encode :: BS.ByteString -> BS.ByteString
encode bs = let
        (first, rest) = b64encode_part bs
        Just fin = b64encode_final rest
    in if BS.null bs
        then BS.empty
        else first `BS.append` fin

decode :: BS.ByteString -> BS.ByteString
decode bs = let
        Right (first, rest) = b64decode_part bs
        Just fin = b64decode_final rest
    in if BS.null bs
        then BS.empty
        else first `BS.append` fin
