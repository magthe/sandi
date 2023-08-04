{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright: (c) Magnus Therning, 2013
-- License: BSD3, found in the LICENSE file

module Codec.Binary.Base85Test where

import Codec.Binary.Base85 qualified as B85

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
    BS.empty @=? B85.encode BS.empty
    BSC.pack "Ac" @=? B85.encode (BSC.pack "f")
    BSC.pack "Ao@" @=? B85.encode (BSC.pack "fo")
    BSC.pack "AoDS" @=? B85.encode (BSC.pack "foo")
    BSC.pack "AoDTs" @=? B85.encode (BSC.pack "foob")
    BSC.pack "AoDTs@/" @=? B85.encode (BSC.pack "fooba")
    BSC.pack "AoDTs@<)" @=? B85.encode (BSC.pack "foobar")

case_enc_specials :: IO ()
case_enc_specials = do
    -- all zero
    BSC.pack "z" @=? B85.encode (BS.pack [0, 0, 0, 0])
    -- all space
    BSC.pack "y" @=? B85.encode (BS.pack [32, 32, 32, 32])
    -- double special
    BSC.pack "yz" @=? B85.encode (BS.pack [32, 32, 32, 32, 0, 0, 0, 0])

case_dec_foobar :: IO ()
case_dec_foobar = do
    -- foobar
    Right BS.empty @=? B85.decode BS.empty
    Right (BSC.pack "f") @=? B85.decode (BSC.pack "Ac")
    Right (BSC.pack "fo") @=? B85.decode (BSC.pack "Ao@")
    Right (BSC.pack "foo") @=? B85.decode (BSC.pack "AoDS")
    Right (BSC.pack "foob") @=? B85.decode (BSC.pack "AoDTs")
    Right (BSC.pack "fooba") @=? B85.decode (BSC.pack "AoDTs@/")
    Right (BSC.pack "foobar") @=? B85.decode (BSC.pack "AoDTs@<)")

case_dec_specials :: IO ()
case_dec_specials = do
    -- all zero
    Right (BS.pack [0, 0, 0, 0]) @=? B85.decode (BSC.pack "z")
    -- all space
    Right (BS.pack [32, 32, 32, 32]) @=? B85.decode (BSC.pack "y")
    -- double special
    Right (BS.pack [32, 32, 32, 32, 0, 0, 0, 0]) @=? B85.decode (BSC.pack "yz")

prop_encdec :: [Word8] -> Bool
prop_encdec ws = BS.pack ws == fromRight undefined (B85.decode $ B85.encode $ BS.pack ws)

tests :: TestTree
tests = $(testGroupGenerator)
