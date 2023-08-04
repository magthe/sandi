{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright: (c) Magnus Therning, 2013
-- License: BSD3, found in the LICENSE file

module Codec.Binary.Base32HexTest where

import Codec.Binary.Base32Hex qualified as B32H

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Either (fromRight)
import Data.Word (Word8)

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH (testGroupGenerator)

case_enc_foobar :: IO ()
case_enc_foobar = do
    BSC.empty @=? B32H.encode BS.empty
    BSC.pack "CO======" @=? B32H.encode (BSC.pack "f")
    BSC.pack "CPNG====" @=? B32H.encode (BSC.pack "fo")
    BSC.pack "CPNMU===" @=? B32H.encode (BSC.pack "foo")
    BSC.pack "CPNMUOG=" @=? B32H.encode (BSC.pack "foob")
    BSC.pack "CPNMUOJ1" @=? B32H.encode (BSC.pack "fooba")
    BSC.pack "CPNMUOJ1E8======" @=? B32H.encode (BSC.pack "foobar")

case_dec_foobar :: IO ()
case_dec_foobar = do
    Right BS.empty @=? B32H.decode BS.empty
    Right (BSC.pack "f") @=? B32H.decode (BSC.pack "CO======")
    Right (BSC.pack "fo") @=? B32H.decode (BSC.pack "CPNG====")
    Right (BSC.pack "foo") @=? B32H.decode (BSC.pack "CPNMU===")
    Right (BSC.pack "foob") @=? B32H.decode (BSC.pack "CPNMUOG=")
    Right (BSC.pack "fooba") @=? B32H.decode (BSC.pack "CPNMUOJ1")
    Right (BSC.pack "foobar") @=? B32H.decode (BSC.pack "CPNMUOJ1E8======")

case_dec_failures :: IO ()
case_dec_failures = do
    --  illegal char
    Left (BS.empty, BSC.pack "C=NMUOJ1") @=? B32H.b32hDecodePart (BSC.pack "C=NMUOJ1")
    -- full block
    Nothing @=? B32H.b32hDecodeFinal (BSC.pack "CPNMUOJ1")
    -- too short
    Nothing @=? B32H.b32hDecodeFinal (BSC.pack "CPNMUO=")

prop_encdec :: [Word8] -> Bool
prop_encdec ws = BS.pack ws == fromRight undefined (B32H.decode $ B32H.encode $ BS.pack ws)

tests :: TestTree
tests = $(testGroupGenerator)
