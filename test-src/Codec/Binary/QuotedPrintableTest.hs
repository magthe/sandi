{-# OPTIONS_GHC -XTemplateHaskell #-}
-- Copyright: (c) Magnus Therning, 2013
-- License: BSD3, found in the LICENSE file

module Codec.Binary.QuotedPrintableTest where

import Codec.TestUtils
import qualified Codec.Binary.QuotedPrintable as QP

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word8)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

case_enc_foobar :: IO ()
case_enc_foobar = do
    BS.empty          @=? QP.encode BS.empty
    BSC.pack "foobar" @=? QP.encode (BSC.pack "foobar")

case_dec_foobar :: IO ()
case_dec_foobar = do
    Right BS.empty            @=? QP.decode BS.empty
    Right (BSC.pack "foobar") @=? QP.decode (BSC.pack "foobar")

prop_encdec :: [Word8] -> Bool
prop_encdec ws = (BS.pack ws) == (fromRight $ QP.decode $ QP.encode $ BS.pack ws)

tests :: TestTree
tests = $(testGroupGenerator)
