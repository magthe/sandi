{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright: (c) Magnus Therning, 2013
-- License: BSD3, found in the LICENSE file

module Codec.Binary.UuTest where

import Codec.Binary.Uu qualified as Uu

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
    BS.empty @=? Uu.encode BS.empty
    BSC.pack "9@" @=? Uu.encode (BSC.pack "f")
    BSC.pack "9F\\" @=? Uu.encode (BSC.pack "fo")
    BSC.pack "9F]O" @=? Uu.encode (BSC.pack "foo")
    BSC.pack "9F]O8@" @=? Uu.encode (BSC.pack "foob")
    BSC.pack "9F]O8F$" @=? Uu.encode (BSC.pack "fooba")
    BSC.pack "9F]O8F%R" @=? Uu.encode (BSC.pack "foobar")

case_dec_foobar :: IO ()
case_dec_foobar = do
    Right BS.empty @=? Uu.decode BS.empty
    Right (BSC.pack "f") @=? Uu.decode (BSC.pack "9@")
    Right (BSC.pack "fo") @=? Uu.decode (BSC.pack "9F\\")
    Right (BSC.pack "foo") @=? Uu.decode (BSC.pack "9F]O")
    Right (BSC.pack "foob") @=? Uu.decode (BSC.pack "9F]O8@")
    Right (BSC.pack "fooba") @=? Uu.decode (BSC.pack "9F]O8F$")
    Right (BSC.pack "foobar") @=? Uu.decode (BSC.pack "9F]O8F%R")

prop_encdec :: [Word8] -> Bool
prop_encdec ws = BS.pack ws == fromRight undefined (Uu.decode $ Uu.encode $ BS.pack ws)

tests :: TestTree
tests = $(testGroupGenerator)
