{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Codec.Binary.Base85
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
module Codec.Binary.Base85
   ( b85_encode_part
   , b85_encode_final
   , b85_decode_part
   , b85_decode_final
   , encode
   , decode
   ) where

import qualified Data.ByteString as BS
import Foreign
import Foreign.C.Types
import System.IO.Unsafe as U
import Data.ByteString.Unsafe

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum

foreign import ccall "static b85.h b85_enc_part"
    c_b85_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static b85.h b85_enc_final"
    c_b85_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import ccall "static b85.h b85_dec_part"
    c_b85_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall "static b85.h b85_dec_final"
    c_b85_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

b85_encode_part :: BS.ByteString -> (BS.ByteString, BS.ByteString)
b85_encode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 4 * 5
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                c_b85_enc_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

b85_encode_final :: BS.ByteString -> Maybe BS.ByteString
b85_encode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 5
    alloca $ \ pOutLen -> do
        r <- c_b85_enc_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r /= 0
            then return Nothing
            else do
                outLen <- peek pOutLen
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return $ Just outBs

b85_decode_part :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
b85_decode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = max 4 $ inLen `div` 5 * 4
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                r <- c_b85_dec_part (castPtr inBuf) (castEnum inLen)
                    outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                if r == 0
                    then return $ Right (outBs, remBs)
                    else return $ Left (outBs, remBs)

b85_decode_final :: BS.ByteString -> Maybe BS.ByteString
b85_decode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 3
    alloca $ \ pOutLen -> do
        r <- c_b85_dec_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r /= 0
            then return Nothing
            else do
                outLen <- peek pOutLen
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return $ Just outBs

encode :: BS.ByteString -> BS.ByteString
encode bs = let
        (first, rest) = b85_encode_part bs
        Just final = b85_encode_final rest
    in first `BS.append` final

decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode bs = let
        iterateDecode bss rem = case b85_decode_part rem of
            Right (d, r) ->
                if BS.null d
                    then Right (BS.concat (reverse bss), r)
                    else iterateDecode (d : bss) r
            Left (d, r) -> Left (BS.concat $ reverse $ d : bss, r)

        handleFinal a@(first, rest) = maybe
            (Left a)
            (\ final -> Right (first `BS.append` final))
            (b85_decode_final rest)

    in either
            Left
            handleFinal
            (iterateDecode [] bs)
