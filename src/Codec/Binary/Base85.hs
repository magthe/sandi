{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module: Codec.Binary.Base85
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
--
-- Implemented as described at <http://en.wikipedia.org/wiki/Ascii85>.
module Codec.Binary.Base85
   ( b85_encode_part
   , b85_encode_final
   , b85_decode_part
   , b85_decode_final
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

foreign import ccall "static b85.h b85_enc_part"
    c_b85_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall "static b85.h b85_enc_final"
    c_b85_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import ccall "static b85.h b85_dec_part"
    c_b85_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall "static b85.h b85_dec_final"
    c_b85_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

-- | Encoding function.
--
-- Encodes as large a part as possible of the indata.
--
-- >>> b85_encode_part $ Data.ByteString.Char8.pack "foobar"
-- ("AoDTs","ar")
--
-- It supports special handling of both all-zero groups and all-space groups.
--
-- >>> b85_encode_part $ Data.ByteString.Char8.pack "    "
-- ("y", "")
-- >>> b85_encode_part $ Data.ByteString.Char8.pack "\0\0\0\0"
-- ("z", "")
b85_encode_part :: BS.ByteString -> (BS.ByteString, BS.ByteString)
b85_encode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
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
-- >>> b85_encode_final $ Data.ByteString.Char8.pack "ar"
-- Just "@<)"
b85_encode_final :: BS.ByteString -> Maybe BS.ByteString
b85_encode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
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
-- >>> b85_decode_part $ Data.ByteString.Char8.pack "AoDTs"
-- Right ("foob","")
-- >>> b85_decode_part $ Data.ByteString.Char8.pack "AoDTs@<)"
-- Right ("foob","@<)")
-- >>> b85_decode_part $ Data.ByteString.Char8.pack "@<)"
-- Right ("","@<)")
--
-- At least 512 bytes of data is allocated for the output, but because of the
-- special handling of all-zero and all-space groups it is possible that the
-- space won't be enough.  (To be sure to always fit the output one would have
-- to allocate 5 times the length of the input.  It seemed a good trade-off to
-- sometimes have to call the function more than once instead.)
--
-- >>> either snd snd $ b85_decode_part $ Data.ByteString.Char8.pack $ Prelude.take 129 $ repeat 'y'
-- "y"
b85_decode_part :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
b85_decode_part bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
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
-- >>> b85_decode_final $ Data.ByteString.Char8.pack "@<)"
-- Just "ar"
-- >>> b85_decode_final $ Data.ByteString.Char8.pack ""
-- Just ""
-- >>> b85_decode_final $ Data.ByteString.Char8.pack "AoDTs"
-- Nothing
b85_decode_final :: BS.ByteString -> Maybe BS.ByteString
b85_decode_final bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
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
encode bs = let
        (first, rest) = b85_encode_part bs
        Just final = b85_encode_final rest
    in first `BS.append` final

-- | Convenience function that combines 'b85_decode_part' and
-- 'b85_decode_final' to decode a complete string.
--
-- >>> decode  $ Data.ByteString.Char8.pack "AoDTs"
-- "foob"
-- >>> encode  $ Data.ByteString.Char8.pack "AoDTs@<)"
-- "foobar"
decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode bs = let
        iterateDecode bss rem = case b85_decode_part rem of
            Right (d, r) ->
                if BS.null d
                    then Right (BS.concat (reverse bss), r)
                    else iterateDecode (d : bss) r
            Left (d, r) -> Left (BS.concat $ reverse $ d : bss, r)

        handleFinal a@(first, rest) = maybe
            (Left a)
            (\ final -> Right (first `BS.append` final))
            (b85_decode_final rest)

    in either
            Left
            handleFinal
            (iterateDecode [] bs)
