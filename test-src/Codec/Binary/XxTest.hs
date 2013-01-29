{-# OPTIONS_GHC -XTemplateHaskell #-}
-- Copyright: (c) Magnus Therning, 2013
-- License: BSD3, found in the LICENSE file

module Codec.Binary.XxTest where

import Codec.TestUtils
import qualified Codec.Binary.Xx as Xx

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
    BS.empty            @=? Xx.encode BS.empty
    BSC.pack "NU"       @=? Xx.encode (BSC.pack "f")
    BSC.pack "Naw"      @=? Xx.encode (BSC.pack "fo")
    BSC.pack "Naxj"     @=? Xx.encode (BSC.pack "foo")
    BSC.pack "NaxjMU"   @=? Xx.encode (BSC.pack "foob")
    BSC.pack "NaxjMa2"  @=? Xx.encode (BSC.pack "fooba")
    BSC.pack "NaxjMa3m" @=? Xx.encode (BSC.pack "foobar")

case_dec_foobar :: IO ()
case_dec_foobar = do
    Right BS.empty            @=? Xx.decode BS.empty
    Right (BSC.pack "f")      @=? Xx.decode (BSC.pack "NU")
    Right (BSC.pack "fo")     @=? Xx.decode (BSC.pack "Naw")
    Right (BSC.pack "foo")    @=? Xx.decode (BSC.pack "Naxj")
    Right (BSC.pack "foob")   @=? Xx.decode (BSC.pack "NaxjMU")
    Right (BSC.pack "fooba")  @=? Xx.decode (BSC.pack "NaxjMa2")
    Right (BSC.pack "foobar") @=? Xx.decode (BSC.pack "NaxjMa3m")

prop_encdec :: [Word8] -> Bool
prop_encdec ws = (BS.pack ws) == (fromRight $ Xx.decode $ Xx.encode $ BS.pack ws)

tests :: Test.Framework.Test
tests = $(testGroupGenerator)
