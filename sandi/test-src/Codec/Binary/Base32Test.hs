{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright: (c) Magnus Therning, 2013
-- License: BSD3, found in the LICENSE file

module Codec.Binary.Base32Test where

import Codec.Binary.Base32 qualified as B32

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Word (Word8)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

case_enc_foobar :: IO ()
case_enc_foobar = do
    BSC.empty @=? B32.encode BSC.empty
    BSC.pack "MY======" @=? B32.encode (BSC.pack "f")
    BSC.pack "MZXQ====" @=? B32.encode (BSC.pack "fo")
    BSC.pack "MZXW6===" @=? B32.encode (BSC.pack "foo")
    BSC.pack "MZXW6YQ=" @=? B32.encode (BSC.pack "foob")
    BSC.pack "MZXW6YTB" @=? B32.encode (BSC.pack "fooba")
    BSC.pack "MZXW6YTBOI======" @=? B32.encode (BSC.pack "foobar")

case_dec_foobar :: IO ()
case_dec_foobar = do
    Right BS.empty @=? B32.decode BS.empty
    Right (BSC.pack "f") @=? B32.decode (BSC.pack "MY======")
    Right (BSC.pack "fo") @=? B32.decode (BSC.pack "MZXQ====")
    Right (BSC.pack "foo") @=? B32.decode (BSC.pack "MZXW6===")
    Right (BSC.pack "foob") @=? B32.decode (BSC.pack "MZXW6YQ=")
    Right (BSC.pack "fooba") @=? B32.decode (BSC.pack "MZXW6YTB")
    Right (BSC.pack "foobar") @=? B32.decode (BSC.pack "MZXW6YTBOI======")

case_dec_failures :: IO ()
case_dec_failures = do
    -- illegal char
    Left (BSC.empty, BSC.pack "M=XW6YTB") @=? B32.b32DecodePart (BSC.pack "M=XW6YTB")
    -- full block
    Nothing @=? B32.b32DecodeFinal (BSC.pack "MZXW6YTB")
    -- too short
    Nothing @=? B32.b32DecodeFinal (BSC.pack "MZXW6Y=")

prop_encdec :: [Word8] -> Bool
prop_encdec ws = BS.pack ws == fromRight undefined (B32.decode $ B32.encode $ BS.pack ws)

tests :: TestTree
tests = $(testGroupGenerator)
