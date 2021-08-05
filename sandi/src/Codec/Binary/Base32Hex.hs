{-# LANGUAGE CApiFFI #-}

-- |
-- Module    : Codec.Binary.Base32Hex
-- Copyright : (c) 2012 Magnus Therning
-- License   : BSD3
--
-- Implemented as specified in RFC 4648 (<http://tools.ietf.org/html/rfc4648>).
--
-- This encoding is closely related to base 32 and so is its implementation, so
-- please refer to "Codec.Binary.Base32" for further details.
module Codec.Binary.Base32Hex
   ( b32hEncodePart
   , b32hEncodeFinal
   , b32hDecodePart
   , b32hDecodeFinal
   , encode
   , decode
   ) where

import Foreign
import Foreign.C.Types
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe
import System.IO.Unsafe as U

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum

foreign import capi "codec.h b32h_enc_part"
    c_b32h_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import capi "codec.h b32h_enc_final"
    c_b32h_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import capi "codec.h b32h_dec_part"
    c_b32h_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import capi "codec.h b32h_dec_final"
    c_b32h_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

-- | Encoding function.
--
-- See 'Codec.Binary.Base32.b32_encode_part'.
--
-- >>> b32hEncodePart $ Data.ByteString.Char8.pack "fooba"
-- ("CPNMUOJ1","")
-- >>> b32hEncodePart $ Data.ByteString.Char8.pack "foobar"
-- ("CPNMUOJ1","r")
b32hEncodePart :: BS.ByteString -> (BS.ByteString, BS.ByteString)
b32hEncodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 5 * 8
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                c_b32h_enc_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

-- | Encoding function for the final block.
--
-- See 'Codec.Binary.Base32.b32_encode_final'.
--
-- >>> b32hEncodeFinal $ Data.ByteString.Char8.pack "r"
-- Just "E8======"
-- >>> b32hEncodeFinal $ Data.ByteString.Char8.pack "fooba"
-- Nothing
b32hEncodeFinal :: BS.ByteString -> Maybe BS.ByteString
b32hEncodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 8
    alloca $ \ pOutLen -> do
        r <- c_b32h_enc_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

-- | Decoding function.
--
-- See 'Codec.Binary.Base32.b32_decode_part'.
--
-- >>> b32hDecodePart $ Data.ByteString.Char8.pack "CPNMUOJ1"
-- Right ("fooba","")
-- >>> b32hDecodePart $ Data.ByteString.Char8.pack "CPNMUOJ1E8======"
-- Right ("fooba","E8======")
-- >>> b32hDecodePart $ Data.ByteString.Char8.pack "C=NMUOJ1"
-- Left ("","C=NMUOJ1")
b32hDecodePart :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
b32hDecodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 8 * 5
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                r <- c_b32h_dec_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
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
-- See 'Codec.Binary.Base32.b32_decode_final'.
--
-- >>> b32hDecodeFinal $ Data.ByteString.Char8.pack "CPNMUOG="
-- Just "foob"
-- >>> b32hDecodeFinal $ Data.ByteString.Char8.pack ""
-- Just ""
-- >>> b32hDecodeFinal $ Data.ByteString.Char8.pack "CPNMUO="
-- Nothing
-- >>> b32hDecodeFinal $ encode $ Data.ByteString.Char8.pack "fooba"
-- Nothing
b32hDecodeFinal :: BS.ByteString -> Maybe BS.ByteString
b32hDecodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 5
    alloca $ \ pOutLen -> do
        r <- c_b32h_dec_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

-- | Convenience function that combines 'b32h_encode_part' and
-- 'b32h_encode_final' to encode a complete string.
--
-- >>> encode $ Data.ByteString.Char8.pack "fooba"
-- "CPNMUOJ1"
-- >>> encode $ Data.ByteString.Char8.pack "foobar"
-- "CPNMUOJ1E8======"
encode :: BS.ByteString -> BS.ByteString
encode bs = first `BS.append` final
    where
        (first, rest) = b32hEncodePart bs
        Just final = b32hEncodeFinal rest

-- | Convenience function that combines 'b32h_decode_part' and
-- 'b32h_decode_final' to decode a complete string.
--
-- >>> decode $ Data.ByteString.Char8.pack "CPNMUOJ1"
-- Right "fooba"
-- >>> decode $ Data.ByteString.Char8.pack "CPNMUOJ1E8======"
-- Right "foobar"
--
-- Failures when decoding returns the decoded part and the remainder:
--
-- >>> decode $ Data.ByteString.Char8.pack "CPNMUOJ1=8======"
-- Left ("fooba","=8======")
decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode bs = either
    Left
    (\ (first, rest) ->
        maybe
            (Left (first, rest))
            (\ fin -> Right (first `BS.append` fin))
            (b32hDecodeFinal rest))
    (b32hDecodePart bs)
