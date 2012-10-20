{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Codec.Binary.Yenc
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
module Codec.Binary.Yenc
    ( y_enc
    , y_dec
    , encode
    , decode
    ) where

import qualified Data.ByteString as BS
import Foreign
import Foreign.C.Types
import System.IO.Unsafe as U
import qualified Data.ByteString.Unsafe as BSU
import Data.List

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum

foreign import ccall "static yenc.h y_enc"
    c_y_enc :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static yenc.h y_dec"
    c_y_dec :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

y_enc :: BS.ByteString -> (BS.ByteString, BS.ByteString)
y_enc bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = ceiling $ (toRational inLen) * 1.2
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                c_y_enc (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- BSU.unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

y_dec :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
y_dec bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes inLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum inLen)
                _ <- c_y_dec (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- BSU.unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free outBuf)
                if remLen == 0
                    then return $ Right outBs
                    else return $ Left (outBs, remBs)

encode :: BS.ByteString -> BS.ByteString
encode = BS.concat . takeWhile (not . BS.null) . unfoldr (Just . y_enc)

decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode = y_dec
