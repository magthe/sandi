{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module: Codec.Binary.Uu
Copyright: (c) 2012 Magnus Therning
License: BSD3

Uuencoding is notoriously badly specified.  This implementation aims at
being compatible with the GNU Sharutils
(<http://www.gnu.org/software/sharutils/>).

Just like Base64 encoding uuencoding expands blocks of 3 bytes into blocks
of 4 bytes.  There is however no well defined ending to a piece of encoded
data, instead uuencoded data is commonly transferred linewise where each
line is prepended with the length of the data in the line.

This module currently only deals with the encoding.  Chopping the encoded
data into lines, and unchopping lines into encoded data is left as an
exercise to the reader.  (Patches are welcome.)
-}
module Codec.Binary.Uu (
    uuEncodePart,
    uuEncodeFinal,
    uuDecodePart,
    uuDecodeFinal,
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

foreign import ccall "static uu.h uu_enc_part"
    c_uu_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static uu.h uu_enc_final"
    c_uu_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import ccall "static uu.h uu_dec_part"
    c_uu_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall "static uu.h uu_dec_final"
    c_uu_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

{- | Encoding function.

This function encodes as large a portion of the input as possible and
returns the encoded part together with the remaining part.  Enough space is
allocated for the encoding to make sure that the remaining part is less than
3 bytes long, which means it can be passed to 'uu_encode_final' as is.

>>> uuEncodePart $ Data.ByteString.Char8.pack "foo"
("9F]O","")
>>> uuEncodePart $ Data.ByteString.Char8.pack "foob"
("9F]O","b")
-}
uuEncodePart :: BS.ByteString -> (BS.ByteString, BS.ByteString)
uuEncodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \(inBuf, inLen) -> do
    let maxOutLen = inLen `div` 3 * 4
    outBuf <- mallocBytes maxOutLen
    alloca $ \pOutLen ->
        alloca $ \pRemBuf ->
            alloca $ \pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                c_uu_enc_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

{- | Encoding function for the final block.

The final block has to have a size less than 3.

>>> uuEncodeFinal $ Data.ByteString.Char8.pack "r"
Just "<@"

Trying to pass in too large a block result in failure:

>>> uuEncodeFinal $ Data.ByteString.Char8.pack "foo"
Nothing
-}
uuEncodeFinal :: BS.ByteString -> Maybe BS.ByteString
uuEncodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \(inBuf, inLen) -> do
    outBuf <- mallocBytes 4
    alloca $ \pOutLen -> do
        r <- c_uu_enc_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

{- | Decoding function.

Decode as large a portion of the input as possible.  Enough data is
allocated for the output to ensure that the remainder is less than 4 bytes
in size.  Success result in a @Right@ value:

>>> uuDecodePart $ Data.ByteString.Char8.pack "9F]O"
Right ("foo","")
>>> uuDecodePart $ Data.ByteString.Char8.pack "9F]O8F$"
Right ("foo","8F$")

Failures occur on bad input and result in a @Left@ value:

>>> uuDecodePart $ Data.ByteString.Char8.pack "9F 0"
Left ("","9F 0")
-}
uuDecodePart :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
uuDecodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \(inBuf, inLen) -> do
    let maxOutLen = inLen `div` 4 * 3
    outBuf <- mallocBytes maxOutLen
    alloca $ \pOutLen ->
        alloca $ \pRemBuf ->
            alloca $ \pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                r <- c_uu_dec_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
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

The final block has to have a size of 0 or 4:

>>> uuDecodeFinal $ Data.ByteString.Char8.pack "9F\\"
Just "fo"
>>> uuDecodeFinal $ Data.ByteString.Char8.pack ""
Just ""
>>> uuDecodeFinal $ Data.ByteString.Char8.pack "9F¬"
Nothing

But it must be the encoding of a block that is less than 3 bytes:

>>> uuDecodeFinal $ encode $ Data.ByteString.Char8.pack "foo"
Nothing
-}
uuDecodeFinal :: BS.ByteString -> Maybe BS.ByteString
uuDecodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \(inBuf, inLen) -> do
    outBuf <- mallocBytes 3
    alloca $ \pOutLen -> do
        r <- c_uu_dec_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

{- | Convenience function that combines 'uu_encode_part' and
'uu_encode_final' to encode a complete string.

>>> encode $ Data.ByteString.Char8.pack "foo"
"9F]O"
>>> encode $ Data.ByteString.Char8.pack "foobar"
"9F]O8F%R"
-}
encode :: BS.ByteString -> BS.ByteString
encode bs = first `BS.append` final
  where
    (first, rest) = uuEncodePart bs
    final = fromJust $ uuEncodeFinal rest

{- | Convenience function that combines 'uu_decode_part' and
'uu_decode_final' to decode a complete string.

>>> decode $ Data.ByteString.Char8.pack "9F]O"
Right "foo"
>>> decode $ Data.ByteString.Char8.pack "9F]O8F%R"
Right "foobar"

Failures when decoding returns the decoded part and the remainder:

>>> decode $ Data.ByteString.Char8.pack "9F]O8F¬R"
Left ("foo","8F\172R")
-}
decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode bs =
    uuDecodePart bs
        >>= \(first, rest) ->
            maybe
                (Left (first, rest))
                (\fin -> Right (first `BS.append` fin))
                (uuDecodeFinal rest)
