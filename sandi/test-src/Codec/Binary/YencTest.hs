{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright: (c) Magnus Therning, 2013
-- License: BSD3, found in the LICENSE file

module Codec.Binary.YencTest where

import Codec.Binary.Yenc qualified as Y

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
    BS.empty @=? Y.encode BS.empty
    BS.pack [144] @=? Y.encode (BSC.pack "f")
    BS.pack [144, 153] @=? Y.encode (BSC.pack "fo")
    BS.pack [144, 153, 153] @=? Y.encode (BSC.pack "foo")
    BS.pack [144, 153, 153, 140] @=? Y.encode (BSC.pack "foob")
    BS.pack [144, 153, 153, 140, 139] @=? Y.encode (BSC.pack "fooba")
    BS.pack [144, 153, 153, 140, 139, 156] @=? Y.encode (BSC.pack "foobar")

case_enc_specials :: IO ()
case_enc_specials = do
    -- expanded chars
    BS.pack [61, 64] @=? Y.encode (BS.pack [214])
    BS.pack [61, 74] @=? Y.encode (BS.pack [224])
    BS.pack [61, 77] @=? Y.encode (BS.pack [227])
    BS.pack [61, 125] @=? Y.encode (BS.pack [19])

case_dec_foobar :: IO ()
case_dec_foobar = do
    Right BS.empty @=? Y.decode BS.empty
    Right (BSC.pack "f") @=? Y.decode (BS.pack [144])
    Right (BSC.pack "fo") @=? Y.decode (BS.pack [144, 153])
    Right (BSC.pack "foo") @=? Y.decode (BS.pack [144, 153, 153])
    Right (BSC.pack "foob") @=? Y.decode (BS.pack [144, 153, 153, 140])
    Right (BSC.pack "fooba") @=? Y.decode (BS.pack [144, 153, 153, 140, 139])
    Right (BSC.pack "foobar") @=? Y.decode (BS.pack [144, 153, 153, 140, 139, 156])

case_dec_specials :: IO ()
case_dec_specials = do
    -- expanded chars
    Right (BS.pack [214]) @=? Y.decode (BS.pack [61, 64])
    Right (BS.pack [224]) @=? Y.decode (BS.pack [61, 74])
    Right (BS.pack [227]) @=? Y.decode (BS.pack [61, 77])
    Right (BS.pack [19]) @=? Y.decode (BS.pack [61, 125])

prop_encdec :: [Word8] -> Bool
prop_encdec ws = BS.pack ws == fromRight undefined (Y.decode $ Y.encode $ BS.pack ws)

tests :: TestTree
tests = $(testGroupGenerator)
