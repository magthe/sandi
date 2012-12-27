{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Codec.Binary.Yenc
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
--
-- Implementation based on the specification found at
-- <http://yence.sourceforge.net/docs/protocol/version1_3_draft.html>.
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

-- | Encoding function.
--
-- This function allocates enough space to hold 20% more than the size of the
-- indata (or at least 512 bytes) and then encodes as much as possible of the
-- indata.  That means there is a risk that the encoded data won't fit and in
-- that case the second part of the pair contains the remainder of the indata.
--
-- >>> y_enc $ Data.ByteString.Char8.pack "foobar"
-- ("\144\153\153\140\139\156","")
-- >>> snd $ y_enc $ Data.ByteString.Char8.pack $ Data.List.take 257 $ repeat '\x13'
-- "\DC3"
y_enc :: BS.ByteString -> (BS.ByteString, BS.ByteString)
y_enc bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = max 512 (ceiling $ (toRational inLen) * 1.2)
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
                outBs <- BSU.unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return (outBs, remBs)

-- | Decoding function.
--
-- >>> y_dec $ Data.ByteString.pack [144,153,153,140,139,156]
-- Right "foobar"
-- >>> y_dec $ Data.ByteString.Char8.pack "=}"
-- Right "\DC3"
--
-- A @Left@ value is only ever returned on decoding errors.
--
-- >>> y_dec $ Data.ByteString.Char8.pack "="
-- Left ("","=")
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
                outBs <- BSU.unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                if remLen == 0
                    then return $ Right outBs
                    else return $ Left (outBs, remBs)

-- | Convenient function that calls 'y_enc' repeatedly until the whole input
-- data is encoded.
encode :: BS.ByteString -> BS.ByteString
encode = BS.concat . takeWhile (not . BS.null) . unfoldr (Just . y_enc)

-- | A synonym for 'y_dec'.
decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode = y_dec
