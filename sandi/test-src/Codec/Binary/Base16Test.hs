{-# LANGUAGE TemplateHaskell #-}
-- Copyright: (c) Magnus Therning, 2013
-- License: BSD3, found in the LICENSE file

module Codec.Binary.Base16Test where

import Codec.TestUtils
import qualified Codec.Binary.Base16 as B16

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word8)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

case_b16_enc_foobar :: IO ()
case_b16_enc_foobar = do
    BSC.empty               @=? B16.encode BSC.empty
    BSC.pack "66"           @=? B16.encode (BSC.pack "f")
    BSC.pack "666F"         @=? B16.encode (BSC.pack "fo")
    BSC.pack "666F6F"       @=? B16.encode (BSC.pack "foo")
    BSC.pack "666F6F62"     @=? B16.encode (BSC.pack "foob")
    BSC.pack "666F6F6261"   @=? B16.encode (BSC.pack "fooba")
    BSC.pack "666F6F626172" @=? B16.encode (BSC.pack "foobar")

case_b16_dec_foobar :: IO ()
case_b16_dec_foobar = do
    Right BS.empty            @=? B16.decode BS.empty
    Right (BSC.pack "f")      @=? B16.decode (BSC.pack "66")
    Right (BSC.pack "fo")     @=? B16.decode (BSC.pack "666F")
    Right (BSC.pack "foo")    @=? B16.decode (BSC.pack "666F6F")
    Right (BSC.pack "foob")   @=? B16.decode (BSC.pack "666F6F62")
    Right (BSC.pack "fooba")  @=? B16.decode (BSC.pack "666F6F6261")
    Right (BSC.pack "foobar") @=? B16.decode (BSC.pack "666F6F626172")

case_b16_dec_failure :: IO ()
case_b16_dec_failure =
    -- odd number of input bytes
    Left (BSC.pack "fooba", BS.pack [55]) @=? B16.decode (BS.pack [54,54,54,70,54,70,54,50,54,49,55])

prop_b16_encdec :: [Word8] -> Bool
prop_b16_encdec ws = BS.pack ws == fromRight (B16.decode $ B16.encode $ BS.pack ws)

tests :: TestTree
tests = $(testGroupGenerator)
