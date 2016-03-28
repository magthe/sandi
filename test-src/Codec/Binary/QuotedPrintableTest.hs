{-# LANGUAGE TemplateHaskell #-}
-- Copyright: (c) Magnus Therning, 2013-2015
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
    BSC.pack "foo=20bar" @=? QP.encode (BSC.pack "foo bar")
    BSC.pack "foo=09bar" @=? QP.encode (BSC.pack "foo\tbar")
    BSC.pack "foo=0Dbar" @=? QP.encode (BSC.pack "foo\rbar")
    BSC.pack "foo=0Abar" @=? QP.encode (BSC.pack "foo\nbar")

case_enc_splitting :: IO ()
case_enc_splitting = do
  BSC.pack "=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=\r\n=3D=3D=3D" @=? QP.encode (BSC.pack "===========================")
  (BSC.pack "=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=\r\n=3D=3D=3D", BSC.pack "") @=? QP.qpEncode (BSC.pack "===========================")
  (BSC.pack "=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D", BSC.pack "") @=? QP.qpEncodeSL (BSC.pack "===========================")

case_dec_foobar :: IO ()
case_dec_foobar = do
    Right BS.empty            @=? QP.decode BS.empty
    Right (BSC.pack "foobar") @=? QP.decode (BSC.pack "foobar")
    Right (BSC.pack "foo bar") @=? QP.decode (BSC.pack "foo bar")
    Right (BSC.pack "foo bar") @=? QP.decode (BSC.pack "foo=20bar")
    Right (BSC.pack "foo\tbar") @=? QP.decode (BSC.pack "foo\tbar")
    Right (BSC.pack "foo\tbar") @=? QP.decode (BSC.pack "foo=09bar")
    Right (BSC.pack "foo\r\nbar") @=? QP.decode (BSC.pack "foo\r\nbar")
    Right (BSC.pack "foobar") @=? QP.decode (BSC.pack "foo=\r\nbar")
    Left (BSC.pack "foo", BSC.pack "\nbar") @=? QP.decode (BSC.pack "foo\nbar")
    Left (BSC.pack "foo", BSC.pack "\rbar") @=? QP.decode (BSC.pack "foo\rbar")

prop_encdec :: [Word8] -> Bool
prop_encdec ws = (BS.pack ws) == (fromRight $ QP.decode $ QP.encode $ BS.pack ws)

tests :: TestTree
tests = $(testGroupGenerator)
