{-# LANGUAGE CApiFFI #-}

-- |
-- Module: Codec.Binary.Base85
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
--
-- Implemented as described at <http://en.wikipedia.org/wiki/Ascii85>.
module Codec.Binary.Base85
   ( b85EncodePart
   , b85EncodeFinal
   , b85DecodePart
   , b85DecodeFinal
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

foreign import capi "codec.h b85_enc_part"
    c_b85_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import capi "codec.h b85_enc_final"
    c_b85_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import capi "codec.h b85_dec_part"
    c_b85_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import capi "codec.h b85_dec_final"
    c_b85_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

-- | Encoding function.
--
-- Encodes as large a part as possible of the indata.
--
-- >>> b85EncodePart $ Data.ByteString.Char8.pack "foobar"
-- ("AoDTs","ar")
--
-- It supports special handling of both all-zero groups and all-space groups.
--
-- >>> b85EncodePart $ Data.ByteString.Char8.pack "    "
-- ("y", "")
-- >>> b85EncodePart $ Data.ByteString.Char8.pack "\0\0\0\0"
-- ("z", "")
b85EncodePart :: BS.ByteString -> (BS.ByteString, BS.ByteString)
b85EncodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 4 * 5
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                c_b85_enc_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return (outBs, remBs)

-- | Encoding function for the final block.
--
-- >>> b85EncodeFinal $ Data.ByteString.Char8.pack "ar"
-- Just "@<)"
b85EncodeFinal :: BS.ByteString -> Maybe BS.ByteString
b85EncodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 5
    alloca $ \ pOutLen -> do
        r <- c_b85_enc_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

-- | Decoding function.
--
-- Decode as large a portion of the input as possible.
--
-- >>> b85DecodePart $ Data.ByteString.Char8.pack "AoDTs"
-- Right ("foob","")
-- >>> b85DecodePart $ Data.ByteString.Char8.pack "AoDTs@<)"
-- Right ("foob","@<)")
-- >>> b85DecodePart $ Data.ByteString.Char8.pack "@<)"
-- Right ("","@<)")
--
-- At least 512 bytes of data is allocated for the output, but because of the
-- special handling of all-zero and all-space groups it is possible that the
-- space won't be enough.  (To be sure to always fit the output one would have
-- to allocate 5 times the length of the input.  It seemed a good trade-off to
-- sometimes have to call the function more than once instead.)
--
-- >>> either snd snd $ b85DecodePart $ Data.ByteString.Char8.pack $ Prelude.take 129 $ repeat 'y'
-- "y"
b85DecodePart :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
b85DecodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = max 512 $ inLen `div` 5 * 4
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                r <- c_b85_dec_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
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
-- >>> b85DecodeFinal $ Data.ByteString.Char8.pack "@<)"
-- Just "ar"
-- >>> b85DecodeFinal $ Data.ByteString.Char8.pack ""
-- Just ""
-- >>> b85DecodeFinal $ Data.ByteString.Char8.pack "AoDTs"
-- Nothing
b85DecodeFinal :: BS.ByteString -> Maybe BS.ByteString
b85DecodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 4
    alloca $ \ pOutLen -> do
        r <- c_b85_dec_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

-- | Convenience function that combines 'b85_encode_part' and
-- 'b85_encode_final' to encode a complete string.
--
-- >>> encode  $ Data.ByteString.Char8.pack "foob"
-- "AoDTs"
-- >>> encode  $ Data.ByteString.Char8.pack "foobar"
-- "AoDTs@<)"
encode :: BS.ByteString -> BS.ByteString
encode bs = first `BS.append` final
    where
        (first, rest) = b85EncodePart bs
        Just final = b85EncodeFinal rest

-- | Convenience function that combines 'b85_decode_part' and
-- 'b85_decode_final' to decode a complete string.
--
-- >>> decode  $ Data.ByteString.Char8.pack "AoDTs"
-- "foob"
-- >>> encode  $ Data.ByteString.Char8.pack "AoDTs@<)"
-- "foobar"
decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode bs = either Left handleFinal (iterateDecode [] bs)
    where
        iterateDecode bss re = case b85DecodePart re of
            Right (d, r) ->
                if BS.null d
                    then Right (BS.concat (reverse bss), r)
                    else iterateDecode (d : bss) r
            Left (d, r) -> Left (BS.concat $ reverse $ d : bss, r)

        handleFinal a@(first, rest) = maybe
            (Left a)
            (\ final -> Right (first `BS.append` final))
            (b85DecodeFinal rest)

