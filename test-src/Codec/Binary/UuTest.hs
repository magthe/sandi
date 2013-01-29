{-# OPTIONS_GHC -XTemplateHaskell #-}
-- Copyright: (c) Magnus Therning, 2013
-- License: BSD3, found in the LICENSE file

module Codec.Binary.UuTest where

import Codec.TestUtils
import qualified Codec.Binary.Uu as Uu

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Test.HUnit
import Test.Framework (Test)
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Data.Word (Word8)

case_enc_foobar :: IO ()
case_enc_foobar = do
    BS.empty            @=? Uu.encode BS.empty
    BSC.pack "9@"       @=? Uu.encode (BSC.pack "f")
    BSC.pack "9F\\"     @=? Uu.encode (BSC.pack "fo")
    BSC.pack "9F]O"     @=? Uu.encode (BSC.pack "foo")
    BSC.pack "9F]O8@"   @=? Uu.encode (BSC.pack "foob")
    BSC.pack "9F]O8F$"  @=? Uu.encode (BSC.pack "fooba")
    BSC.pack "9F]O8F%R" @=? Uu.encode (BSC.pack "foobar")

case_dec_foobar :: IO ()
case_dec_foobar = do
    Right BS.empty            @=? Uu.decode BS.empty
    Right (BSC.pack "f")      @=? Uu.decode (BSC.pack "9@")
    Right (BSC.pack "fo")     @=? Uu.decode (BSC.pack "9F\\")
    Right (BSC.pack "foo")    @=? Uu.decode (BSC.pack "9F]O")
    Right (BSC.pack "foob")   @=? Uu.decode (BSC.pack "9F]O8@")
    Right (BSC.pack "fooba")  @=? Uu.decode (BSC.pack "9F]O8F$")
    Right (BSC.pack "foobar") @=? Uu.decode (BSC.pack "9F]O8F%R")

prop_encdec :: [Word8] -> Bool
prop_encdec ws = (BS.pack ws) == (fromRight $ Uu.decode $ Uu.encode $ BS.pack ws)

tests :: Test.Framework.Test
tests = $(testGroupGenerator)
