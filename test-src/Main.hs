{-# OPTIONS_GHC -XTemplateHaskell #-}
module Main where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.ByteString
import qualified Codec.Binary.Base64 as B64
import qualified Codec.Binary.Base64Url as B64U
import qualified Codec.Binary.Base32 as B32

-- {{{1 base64
case_b64encode = do
    -- foobar
    empty @=? B64.encode empty
    pack [90,103,61,61] @=? B64.encode (pack [102])
    pack [90,109,56,61] @=? B64.encode (pack [102,111])
    pack [90,109,57,118] @=? B64.encode (pack [102,111,111])
    pack [90,109,57,118,89,103,61,61] @=? B64.encode (pack [102,111,111,98])
    pack [90,109,57,118,89,109,69,61] @=? B64.encode (pack [102,111,111,98,97])
    pack [90,109,57,118,89,109,70,121] @=? B64.encode (pack [102,111,111,98,97,114])
    -- /++/
    pack [47,43,43,47] @=? B64.encode (pack [255,239,191])

case_b64decode = do
    -- foobar
    Right empty @=? B64.decode empty 
    (Right $ pack [102]) @=? B64.decode (pack [90,103,61,61]) 
    (Right $ pack [102,111]) @=? B64.decode (pack [90,109,56,61]) 
    (Right $ pack [102,111,111]) @=? B64.decode (pack [90,109,57,118]) 
    (Right $ pack [102,111,111,98]) @=? B64.decode (pack [90,109,57,118,89,103,61,61]) 
    (Right $ pack [102,111,111,98,97]) @=? B64.decode (pack [90,109,57,118,89,109,69,61]) 
    (Right $ pack [102,111,111,98,97,114]) @=? B64.decode (pack [90,109,57,118,89,109,70,121]) 
    -- /++/
    (Right $ pack [255,239,191]) @=? B64.decode (pack [47,43,43,47])

-- {{{1 base64url
case_b64urlencode = do
    -- foobar
    empty @=? B64U.encode empty
    pack [90,103,61,61] @=? B64U.encode (pack [102])
    pack [90,109,56,61] @=? B64U.encode (pack [102,111])
    pack [90,109,57,118] @=? B64U.encode (pack [102,111,111])
    pack [90,109,57,118,89,103,61,61] @=? B64U.encode (pack [102,111,111,98])
    pack [90,109,57,118,89,109,69,61] @=? B64U.encode (pack [102,111,111,98,97])
    pack [90,109,57,118,89,109,70,121] @=? B64U.encode (pack [102,111,111,98,97,114])
    -- _--_
    pack [95,45,45,95] @=? B64U.encode (pack [255,239,191])

case_b64urldecode = do
    -- foobar
    Right empty @=? B64U.decode empty 
    (Right $ pack [102]) @=? B64U.decode (pack [90,103,61,61]) 
    (Right $ pack [102,111]) @=? B64U.decode (pack [90,109,56,61]) 
    (Right $ pack [102,111,111]) @=? B64U.decode (pack [90,109,57,118]) 
    (Right $ pack [102,111,111,98]) @=? B64U.decode (pack [90,109,57,118,89,103,61,61]) 
    (Right $ pack [102,111,111,98,97]) @=? B64U.decode (pack [90,109,57,118,89,109,69,61]) 
    (Right $ pack [102,111,111,98,97,114]) @=? B64U.decode (pack [90,109,57,118,89,109,70,121]) 
    -- _--_
    (Right $ pack [255,239,191]) @=? B64U.decode (pack [95,45,45,95])

-- {{{1 base32
case_b32encode = do
    -- foobar
    empty @=? B32.encode empty 
    pack [77,89,61,61,61,61,61,61] @=? B32.encode (pack [102])
    pack [77,90,88,81,61,61,61,61] @=? B32.encode (pack [102,111])
    pack [77,90,88,87,54,61,61,61] @=? B32.encode (pack [102,111,111])
    pack [77,90,88,87,54,89,81,61] @=? B32.encode (pack [102,111,111,98])
    pack [77,90,88,87,54,89,84,66] @=? B32.encode (pack [102,111,111,98,97])
    pack [77,90,88,87,54,89,84,66,79,73,61,61,61,61,61,61] @=? B32.encode (pack [102,111,111,98,97,114])

case_b32decode = do
    -- foobar
    Right empty @=? B32.decode empty 
    (Right $ pack [102]) @=? B32.decode (pack [77,89,61,61,61,61,61,61])
    (Right $ pack [102,111]) @=? B32.decode (pack [77,90,88,81,61,61,61,61])
    (Right $ pack [102,111,111]) @=? B32.decode (pack [77,90,88,87,54,61,61,61])
    (Right $ pack [102,111,111,98]) @=? B32.decode (pack [77,90,88,87,54,89,81,61])
    (Right $ pack [102,111,111,98,97]) @=? B32.decode (pack [77,90,88,87,54,89,84,66])
    (Right $ pack [102,111,111,98,97,114]) @=? B32.decode (pack [77,90,88,87,54,89,84,66,79,73,61,61,61,61,61,61])

-- {{{1 tests & main
tests = [$(testGroupGenerator)]

main = defaultMain tests
