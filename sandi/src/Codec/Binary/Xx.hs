{-# LANGUAGE CApiFFI #-}

-- |
-- Module: Codec.Binary.Xx
-- Copyright: (c) 2012 Magnus Therning
-- License: BSD3
--
-- Xxencoding is obsolete but still included for completeness.  Further
-- information on the encoding can be found at
-- <http://en.wikipedia.org/wiki/Xxencode>.  It should be noted that this
-- implementation performs no padding.
--
-- This encoding is very similar to uuencoding, therefore further information
-- regarding the functions can be found in the documentation of
-- "Codec.Binary.Uu".
module Codec.Binary.Xx
    ( xxEncodePart
    , xxEncodeFinal
    , xxDecodePart
    , xxDecodeFinal
    , encode
    , decode
    ) where

import Data.ByteString.Unsafe
import Foreign
import Foreign.C.Types
import System.IO.Unsafe as U
import qualified Data.ByteString as BS

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum

foreign import capi "codec.h xx_enc_part"
    c_xx_enc_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import capi "codec.h xx_enc_final"
    c_xx_enc_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

foreign import capi "codec.h xx_dec_part"
    c_xx_dec_part :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import capi "codec.h xx_dec_final"
    c_xx_dec_final :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> IO CInt

-- | Encoding function.
--
-- >>> xxEncodePart $ Data.ByteString.Char8.pack "foo"
-- ("Naxj","")
-- >>> xxEncodePart $ Data.ByteString.Char8.pack "foob"
-- ("Naxj","b")
xxEncodePart :: BS.ByteString -> (BS.ByteString, BS.ByteString)
xxEncodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 3 * 4
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                c_xx_enc_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- unsafePackCStringFinalizer outBuf (castEnum outLen) (free outBuf)
                return (outBs, remBs)

-- | Encoding function for the final block.
--
-- >>> xxEncodeFinal $ Data.ByteString.Char8.pack "r"
-- Just "QU"
-- >>> xxEncodeFinal $ Data.ByteString.Char8.pack "foo"
-- Nothing
xxEncodeFinal :: BS.ByteString -> Maybe BS.ByteString
xxEncodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 4
    alloca $ \ pOutLen -> do
        r <- c_xx_enc_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

-- | Decoding function.
--
-- >>> xxDecodePart $ Data.ByteString.Char8.pack "Naxj"
-- Right ("foo","")
-- >>> xxDecodePart $ Data.ByteString.Char8.pack "NaxjMa3"
-- Right ("foo","Ma3")
--
-- >>> xxDecodePart $ Data.ByteString.Char8.pack "Na j"
-- Left ("","Na J")
xxDecodePart :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
xxDecodePart bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    let maxOutLen = inLen `div` 4 * 3
    outBuf <- mallocBytes maxOutLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum maxOutLen)
                r <- c_xx_dec_part (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
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
-- >>> xxDecodeFinal $ Data.ByteString.Char8.pack "Naw"
-- Just "fo"
-- >>> xxDecodeFinal $ Data.ByteString.Char8.pack ""
-- Just ""
-- >>> xxDecodeFinal $ Data.ByteString.Char8.pack "Na "
-- Nothing
--
-- >>> xxDecodeFinal $ encode $ Data.ByteString.Char8.pack "foo"
-- Nothing
xxDecodeFinal :: BS.ByteString -> Maybe BS.ByteString
xxDecodeFinal bs = U.unsafePerformIO $ unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes 3
    alloca $ \ pOutLen -> do
        r <- c_xx_dec_final (castPtr inBuf) (castEnum inLen) outBuf pOutLen
        if r == 0
            then do
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                outBs <- unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                return $ Just outBs
            else free outBuf >> return Nothing

encode :: BS.ByteString -> BS.ByteString
encode bs = first `BS.append` final
    where
        (first, rest) = xxEncodePart bs
        Just final = xxEncodeFinal rest

decode :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
decode bs = either
    Left
    (\ (first, rest) ->
        maybe
            (Left (first, rest))
            (\ fin -> Right (first `BS.append` fin))
            (xxDecodeFinal rest))
    (xxDecodePart bs)
