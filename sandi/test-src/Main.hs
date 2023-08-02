{-# LANGUAGE ImportQualifiedPost #-}

-- Copyright: (c) Magnus Therning, 2012
-- License: BSD3, found in the LICENSE file

module Main where

import Test.Tasty

import Codec.Binary.Base16Test qualified as B16Test
import Codec.Binary.Base32HexTest qualified as B32HTest
import Codec.Binary.Base32Test qualified as B32Test
import Codec.Binary.Base64Test qualified as B64Test
import Codec.Binary.Base64UrlTest qualified as B64UTest
import Codec.Binary.Base85Test qualified as B85Test
import Codec.Binary.QuotedPrintableTest qualified as QPTest
import Codec.Binary.UuTest qualified as UuTest
import Codec.Binary.XxTest qualified as XxTest
import Codec.Binary.YencTest qualified as YTest

tests :: TestTree
tests =
    testGroup
        "All tests"
        [ B16Test.tests
        , B32Test.tests
        , B32HTest.tests
        , B64Test.tests
        , B64UTest.tests
        , B85Test.tests
        , QPTest.tests
        , UuTest.tests
        , XxTest.tests
        , YTest.tests
        ]

main :: IO ()
main = defaultMain tests
