{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Codec.Binary.Base16
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
--
-- Implemention of base 16 encoding (hex encoding) as specified in RFC 4648
-- (<http://tools.ietf.org/html/rfc4648>).
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

-- | Encoding function.
--
-- This function, unlike some other encoding functions in the library, simply
-- cannot fail.  Double the length of the input string is allocated for the
-- encoded data, which is guaranteed to hold the result.
--
-- >>> b16_enc $ Data.ByteString.pack [0x00]
-- "00"
--
-- >>> b16_enc $ Data.ByteString.Char8.pack "foobar"
-- "666F6F626172"
b16_enc :: BS.ByteString
    -> BS.ByteString -- ^ The encoded string
b16_enc bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen * 2
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                c_b16_enc (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                outBs <- BSU.unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return outBs

-- | Decoding function.
--
-- The returned value on success is @Right \<decoded string>@, and on failure
-- it's @Left (\<decoded part\>, \<undecodable part>)@.  Space equal to the
-- length of the input string is allocated, which is more than enough to hold
-- the decoded data.
--
-- >>> b16_dec $ Data.ByteString.Char8.pack "00"
-- Right "\NUL"
--
-- >>> b16_dec $ Data.ByteString.Char8.pack "666F6F626172"
-- Right "foobar"
--
-- >>> b16_dec $ Data.ByteString.Char8.pack "666F6F62617"
-- Left ("fooba","7")
b16_dec :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
b16_dec bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes inLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum inLen)
                r <- c_b16_dec (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- BSU.unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                if r == 0
                    then return $ Right outBs
                    else return $ Left (outBs, remBs)

-- | A synonym for 'b16_enc'.
encode :: BS.ByteString -> BS.ByteString
encode = b16_enc

-- | A synonum for 'b16_dec'.
decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode = b16_dec
