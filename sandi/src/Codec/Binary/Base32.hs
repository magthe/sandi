{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module: Codec.Binary.Base32
Copyright: (c) 2012 Magnus Therning
License: BSD3

Implemented as specified in RFC 4648 (<http://tools.ietf.org/html/rfc4648>).

Base32 encoding works by expanding blocks of 5 bytes of data into blocks of
8 bytes of data.  Finally it also includes a well defined ending of the
encoded data to make sure the size of the final block of encoded data is 8
bytes too.
-}
module Codec.Binary.Base32 (
    b32EncodePart,
    b32EncodeFinal,
    b32DecodePart,
    b32DecodeFinal,
    encode,
    decode,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe
import Data.Maybe (fromJust)
import Foreign
import Foreign.C.Types
import System.IO.Unsafe as U

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum

foreign import ccall "static b32.h b32_enc_part"
    c_b32_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static b32.h b32_enc_final"
    c_b32_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import ccall "static b32.h b32_dec_part"
    c_b32_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall "static b32.h b32_dec_final"
    c_b32_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

{- | Encoding function.

This function encodes as large a portion of the input as possible and
returns the encoded part together with the remaining part.  Enough space is
allocated for the encoding to make sure that the remaining part is less than
5 bytes long, which means it can be passed to 'b32_encode_final' as is.

>>> b32EncodePart $ Data.ByteString.Char8.pack "fooba"
("MZXW6YTB","")
>>> b32EncodePart $ Data.ByteString.Char8.pack "foobar"
("MZXW6YTB","r")
-}
b32EncodePart :: BS.ByteString -> (BS.ByteString, BS.ByteString)
b32EncodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \(inBuf, inLen) -> do
    let maxOutLen = inLen `div` 5 * 8
    outBuf <- mallocBytes maxOutLen
    alloca $ \pOutLen ->
        alloca $ \pRemBuf ->
            alloca $ \pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                c_b32_enc_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

{- | Encoding function for the final block.

The final block has to have a size less than 5.

>>> b32EncodeFinal $ Data.ByteString.Char8.pack "r"
Just "OI======"

Trying to pass in too large a block result in failure:

>>> b32EncodeFinal $ Data.ByteString.Char8.pack "fooba"
Nothing
-}
b32EncodeFinal :: BS.ByteString -> Maybe BS.ByteString
b32EncodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \(inBuf, inLen) -> do
    outBuf <- mallocBytes 8
    alloca $ \pOutLen -> do
        r <- c_b32_enc_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

{- | Decoding function.

Decode as large a portion of the input as possible.  Enough data is
allocated for the output to ensure that the remainder is less than 8 bytes
in size.  Success result in a @Right@ value:

>>> b32DecodePart $ Data.ByteString.Char8.pack "MZXW6YTB"
Right ("fooba","")
>>> b32DecodePart $ Data.ByteString.Char8.pack "MZXW6YTBOI======"
Right ("fooba","OI======")

Failures occur on bad input and result in a @Left@ value:

>>> b32DecodePart $ Data.ByteString.Char8.pack "M=XW6YTB"
Left ("","M=XW6YTB")
-}
b32DecodePart :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
b32DecodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \(inBuf, inLen) -> do
    let maxOutLen = inLen `div` 8 * 5
    outBuf <- mallocBytes maxOutLen
    alloca $ \pOutLen ->
        alloca $ \pRemBuf ->
            alloca $ \pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                r <- c_b32_dec_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                if r == 0
                    then return $ Right (outBs, remBs)
                    else return $ Left (outBs, remBs)

{- | Decoding function for the final block.

The final block has to have a size of 0 or 8:

>>> b32DecodeFinal $ Data.ByteString.Char8.pack "MZXW6YQ="
Just "foob"
>>> b32DecodeFinal $ Data.ByteString.Char8.pack ""
Just ""
>>> b32DecodeFinal $ Data.ByteString.Char8.pack "MZXW6Y="
Nothing

But it must be the encoding of a block that is less than 5 bytes:

>>> b32DecodeFinal $ encode $ Data.ByteString.Char8.pack "fooba"
Nothing
-}
b32DecodeFinal :: BS.ByteString -> Maybe BS.ByteString
b32DecodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \(inBuf, inLen) -> do
    outBuf <- mallocBytes 5
    alloca $ \pOutLen -> do
        r <- c_b32_dec_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

{- | Convenience function that combines 'b32_encode_part' and
'b32_encode_final' to encode a complete string.

>>> encode $ Data.ByteString.Char8.pack "fooba"
"MZXW6YTB"
>>> encode $ Data.ByteString.Char8.pack "foobar"
"MZXW6YTBOI======"
-}
encode :: BS.ByteString -> BS.ByteString
encode bs = first `BS.append` final
  where
    (first, rest) = b32EncodePart bs
    final = fromJust $ b32EncodeFinal rest

{- | Convenience function that combines 'b32_decode_part' and
'b32_decode_final' to decode a complete string.

>>> decode $ Data.ByteString.Char8.pack "MZXW6YTB"
Right "fooba"
>>> decode $ Data.ByteString.Char8.pack "MZXW6YTBOI======"
Right "foobar"

Failures when decoding returns the decoded part and the remainder:

>>> decode $ Data.ByteString.Char8.pack "MZXW6YTBOI=0===="
Left ("fooba","OI=0====")
-}
decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode bs =
    b32DecodePart bs
        >>= \(first, rest) ->
            maybe
                (Left (first, rest))
                (\fin -> Right (first `BS.append` fin))
                (b32DecodeFinal rest)
