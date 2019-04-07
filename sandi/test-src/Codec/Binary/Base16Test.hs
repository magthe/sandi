{-# LANGUAGE TemplateHaskell #-}
-- Copyright: (c) Magnus Therning, 2013
-- License: BSD3, found in the LICENSE file

module Codec.Binary.Base16Test where

import qualified Codec.Binary.Base16 as B16

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Either (fromRight)
import           Data.Word (Word8)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

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

prop_b16_encdec :: [Word8] -> Property
prop_b16_encdec ws =
  BS.pack ws === fromRight undefined (B16.decode $ B16.encode $ BS.pack ws)

prop_b16_enc_length :: [Word8] -> Property
prop_b16_enc_length ws =
  BS.length (B16.encode $ BS.pack ws) === 2 * length ws

allBut1 [x] = []
allBut1 (x:xs) = x : allBut1 xs

b16Chars = ['0' .. '9'] ++ ['A' .. 'F']

newtype B16Str = B16Str String
  deriving (Eq, Show)

instance Arbitrary B16Str where
  arbitrary = B16Str <$> listOf (elements b16Chars)

prop_valid_B16Str :: B16Str -> Bool
prop_valid_B16Str (B16Str s) =
  all (`elem` b16Chars) s

prop_b16_dec_length :: B16Str -> Property
prop_b16_dec_length (B16Str s) =
  checkCoverage $
  cover 40 (even $ length s) "even" $
  cover 40 (odd $ length s) "odd" $
  let r = B16.decode (BSC.pack s)
  in if even $ length s
     then (B16.encode <$> r) === Right (BSC.pack s)
     else r === Left (fromRight undefined (B16.decode . BSC.pack $ allBut1 s), BSC.pack [last s])

tests :: TestTree
tests = $(testGroupGenerator)
