{-# LANGUAGE CApiFFI #-}

-- |
-- Module: Codec.Binary.Base64
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
--
-- Implemented as specified in RFC 4648 (<http://tools.ietf.org/html/rfc4648>).
--
-- Base64 encoding works by expanding blocks of 3 bytes of data into blocks of
-- 4 bytes of data.  Finally it also includes a well defined ending of the
-- encoded data to make sure the size of the final block of encoded data is 4
-- bytes too.
module Codec.Binary.Base64
    ( b64EncodePart
    , b64EncodeFinal
    , b64DecodePart
    , b64DecodeFinal
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

foreign import capi "codec.h b64_enc_part"
    c_b64_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import capi "codec.h b64_enc_final"
    c_b64_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import capi "codec.h b64_dec_part"
    c_b64_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import capi "codec.h b64_dec_final"
    c_b64_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

-- | Encoding function.
--
-- This function encodes as large a portion of the input as possible and
-- returns the encoded part together with the remaining part.  Enough space is
-- allocated for the encoding to make sure that the remaining part is less than
-- 3 bytes long, which means it can be passed to 'b64_encode_final' as is.
--
-- >>> b64EncodePart $ Data.ByteString.Char8.pack "foo"
-- ("Zm9v","")
-- >>> b64EncodePart $ Data.ByteString.Char8.pack "foob"
-- ("Zm9v","b")
b64EncodePart :: BS.ByteString -> (BS.ByteString, BS.ByteString)
b64EncodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 3 * 4
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                c_b64_enc_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

-- | Encoding function for the final block.
--
-- The final block has to have a size less than 3.
--
-- >>> b64EncodeFinal $ Data.ByteString.Char8.pack "r"
-- Just "cg=="
--
-- Trying to pass in too large a block result in failure:
--
-- >>> b64EncodeFinal $ Data.ByteString.Char8.pack "foo"
-- Nothing
b64EncodeFinal :: BS.ByteString -> Maybe BS.ByteString
b64EncodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 4
    alloca $ \ pOutLen -> do
        r <- c_b64_enc_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

-- | Decoding function.
--
-- Decode as large a portion of the input as possible.  Enough data is
-- allocated for the output to ensure that the remainder is less than 4 bytes
-- in size.  Success result in a @Right@ value:
--
-- >>> b64DecodePart $ Data.ByteString.Char8.pack "Zm9v"
-- Right ("foo","")
-- >>> b64DecodePart $ Data.ByteString.Char8.pack "Zm9vYmE="
-- Right ("foo","YmE=")
--
-- Failures occur on bad input and result in a @Left@ value:
--
-- >>> b64DecodePart $ Data.ByteString.Char8.pack "Z=9v"
-- Left ("","Z=9v")
b64DecodePart :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
b64DecodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 4 * 3
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                r <- c_b64_dec_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
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
-- The final block has to have a size of 0 or 4:
--
-- >>> b64DecodeFinal $ Data.ByteString.Char8.pack "Zm8="
-- Just "fo"
-- >>> b64DecodeFinal $ Data.ByteString.Char8.pack ""
-- Just ""
-- >>> b64DecodeFinal $ Data.ByteString.Char8.pack "Zm="
-- Nothing
--
-- But it must be the encoding of a block that is less than 3 bytes:
--
-- >>> b64DecodeFinal $ encode $ Data.ByteString.Char8.pack "foo"
-- Nothing
b64DecodeFinal :: BS.ByteString -> Maybe BS.ByteString
b64DecodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 3
    alloca $ \ pOutLen -> do
        r <- c_b64_dec_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

-- | Convenience function that combines 'b64_encode_part' and
-- 'b64_encode_final' to encode a complete string.
--
-- >>> encode $ Data.ByteString.Char8.pack "foo"
-- "Zm9v"
-- >>> encode $ Data.ByteString.Char8.pack "foobar"
-- "Zm9vYmFy"
encode :: BS.ByteString -> BS.ByteString
encode bs = first `BS.append` final
    where
        (first, rest) = b64EncodePart bs
        Just final = b64EncodeFinal rest

-- | Convenience function that combines 'b64_decode_part' and
-- 'b64_decode_final' to decode a complete string.
--
-- >>> decode $ Data.ByteString.Char8.pack "Zm9v"
-- Right "foo"
-- >>> decode $ Data.ByteString.Char8.pack "Zm9vYmFy"
-- Right "foobar"
--
-- Failures when decoding returns the decoded part and the remainder:
--
-- >>> decode $ Data.ByteString.Char8.pack "Zm9vYm=y"
-- Left ("foo","Ym=y")
decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode bs = either
    Left
    (\ (first, rest) ->
        maybe
            (Left (first, rest))
            (\ fin -> Right (first `BS.append` fin))
            (b64DecodeFinal rest))
    (b64DecodePart bs)
