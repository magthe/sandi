{-# OPTIONS_GHC -XTemplateHaskell #-}
module Main where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.ByteString
import Codec.Binary.Base64

case_b64encode = do
    empty @=? encode empty
    pack [90,103,61,61] @=? encode (pack [102])
    pack [90,109,56,61] @=? encode (pack [102,111])
    pack [90,109,57,118] @=? encode (pack [102,111,111])
    pack [90,109,57,118,89,103,61,61] @=? encode (pack [102,111,111,98])
    pack [90,109,57,118,89,109,69,61] @=? encode (pack [102,111,111,98,97])
    pack [90,109,57,118,89,109,70,121] @=? encode (pack [102,111,111,98,97,114])

case_b64decode = do
    empty @=? decode empty 
    pack [102] @=? decode (pack [90,103,61,61]) 
    pack [102,111] @=? decode (pack [90,109,56,61]) 
    pack [102,111,111] @=? decode (pack [90,109,57,118]) 
    pack [102,111,111,98] @=? decode (pack [90,109,57,118,89,103,61,61]) 
    pack [102,111,111,98,97] @=? decode (pack [90,109,57,118,89,109,69,61]) 
    pack [102,111,111,98,97,114] @=? decode (pack [90,109,57,118,89,109,70,121]) 

tests = [$(testGroupGenerator)]

main = defaultMain tests
