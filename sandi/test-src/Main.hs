-- Copyright: (c) Magnus Therning, 2012
-- License: BSD3, found in the LICENSE file

module Main where

import Test.Tasty

import qualified Codec.Binary.Base16Test as B16Test
import qualified Codec.Binary.Base32Test as B32Test
import qualified Codec.Binary.Base32HexTest as B32HTest
import qualified Codec.Binary.Base64Test as B64Test
import qualified Codec.Binary.Base64UrlTest as B64UTest
import qualified Codec.Binary.Base85Test as B85Test
import qualified Codec.Binary.QuotedPrintableTest as QPTest
import qualified Codec.Binary.UuTest as UuTest
import qualified Codec.Binary.XxTest as XxTest
import qualified Codec.Binary.YencTest as YTest

tests :: TestTree
tests = testGroup "All tests"
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
