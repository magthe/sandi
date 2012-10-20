{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Codec.Binary.Base16
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
module Codec.Binary.Base16
    ( b16_enc
    , b16_dec
    , encode
    , decode
    ) where

import Foreign
import Foreign.C.Types
import System.IO.Unsafe as U
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum

foreign import ccall "static b16.h b16_enc"
    c_b16_enc :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static b16.h b16_dec"
    c_b16_dec :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

b16_enc :: BS.ByteString -> (BS.ByteString, BS.ByteString)
b16_enc bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen * 2
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                c_b16_enc (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- BSU.unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

b16_dec :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
b16_dec bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes inLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum inLen)
                _ <- c_b16_dec (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- BSU.unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                if remLen == 0
                    then return $ Right outBs
                    else return $ Left (outBs, remBs)

encode :: BS.ByteString -> BS.ByteString
encode = fst . b16_enc

decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode = b16_dec
