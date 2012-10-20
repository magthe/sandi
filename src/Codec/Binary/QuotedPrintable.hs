{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Codec.Binary.QuotedPrintable
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
module Codec.Binary.QuotedPrintable
    ( qp_enc
    , qp_dec
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
    c_qp_enc :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static qp.h qp_dec"
    c_qp_dec :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

qp_enc :: BS.ByteString -> (BS.ByteString, BS.ByteString)
qp_enc bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutBuf = 2 * inLen
    outBuf <- mallocBytes maxOutBuf
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutBuf)
                c_qp_enc (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- BSU.unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return (outBs, remBs)

qp_dec :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
qp_dec bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
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
                    then return $ Right outBs
                    else return $ Left (outBs, remBs)

encode :: BS.ByteString -> BS.ByteString
encode = BS.concat . takeWhile (not . BS.null) . unfoldr (Just . qp_enc)

decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode = qp_dec
