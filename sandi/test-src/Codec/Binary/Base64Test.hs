{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright: (c) Magnus Therning, 2013
-- License: BSD3, found in the LICENSE file

module Codec.Binary.Base64Test where

import Codec.Binary.Base64 qualified as B64

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
    BSC.empty @=? B64.encode BSC.empty
    BSC.pack "Zg==" @=? B64.encode (BSC.pack "f")
    BSC.pack "Zm8=" @=? B64.encode (BSC.pack "fo")
    BSC.pack "Zm9v" @=? B64.encode (BSC.pack "foo")
    BSC.pack "Zm9vYg==" @=? B64.encode (BSC.pack "foob")
    BSC.pack "Zm9vYmE=" @=? B64.encode (BSC.pack "fooba")
    BSC.pack "Zm9vYmFy" @=? B64.encode (BSC.pack "foobar")

case_enc_specials :: IO ()
case_enc_specials =
    -- /++/
    BSC.pack "/++/" @=? B64.encode (BS.pack [255, 239, 191])

case_dec_foobar :: IO ()
case_dec_foobar = do
    Right BSC.empty @=? B64.decode BSC.empty
    Right (BSC.pack "f") @=? B64.decode (BSC.pack "Zg==")
    Right (BSC.pack "fo") @=? B64.decode (BSC.pack "Zm8=")
    Right (BSC.pack "foo") @=? B64.decode (BSC.pack "Zm9v")
    Right (BSC.pack "foob") @=? B64.decode (BSC.pack "Zm9vYg==")
    Right (BSC.pack "fooba") @=? B64.decode (BSC.pack "Zm9vYmE=")
    Right (BSC.pack "foobar") @=? B64.decode (BSC.pack "Zm9vYmFy")

case_dec_specials :: IO ()
case_dec_specials =
    -- /++/
    Right (BS.pack [255, 239, 191]) @=? B64.decode (BSC.pack "/++/")

prop_encdec :: [Word8] -> Bool
prop_encdec ws = BS.pack ws == fromRight undefined (B64.decode $ B64.encode $ BS.pack ws)

tests :: TestTree
tests = $(testGroupGenerator)
