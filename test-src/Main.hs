{-# OPTIONS_GHC -XTemplateHaskell #-}

-- Copyright: (c) Magnus Therning, 2012
-- License: BSD3, found in the LICENSE file

module Main where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Codec.Binary.Base16 as B16
import qualified Codec.Binary.Base32 as B32
import qualified Codec.Binary.Base32Hex as B32H
import qualified Codec.Binary.Base64 as B64
import qualified Codec.Binary.Base64Url as B64U
import qualified Codec.Binary.Base85 as B85
import qualified Codec.Binary.QuotedPrintable as QP
import qualified Codec.Binary.Uu as Uu
import qualified Codec.Binary.Xx as Xx
import qualified Codec.Binary.Yenc as Y

-- {{{1 base16
case_b16_enc_foobar = do
    BSC.empty               @=? B16.encode BSC.empty
    BSC.pack "66"           @=? B16.encode (BSC.pack "f")
    BSC.pack "666F"         @=? B16.encode (BSC.pack "fo")
    BSC.pack "666F6F"       @=? B16.encode (BSC.pack "foo")
    BSC.pack "666F6F62"     @=? B16.encode (BSC.pack "foob")
    BSC.pack "666F6F6261"   @=? B16.encode (BSC.pack "fooba")
    BSC.pack "666F6F626172" @=? B16.encode (BSC.pack "foobar")

case_b16_dec_foobar = do
    Right BS.empty                            @=? B16.decode BS.empty
    Right (BSC.pack "f")      @=? B16.decode (BSC.pack "66")
    Right (BSC.pack "fo")     @=? B16.decode (BSC.pack "666F")
    Right (BSC.pack "foo")    @=? B16.decode (BSC.pack "666F6F")
    Right (BSC.pack "foob")   @=? B16.decode (BSC.pack "666F6F62")
    Right (BSC.pack "fooba")  @=? B16.decode (BSC.pack "666F6F6261")
    Right (BSC.pack "foobar") @=? B16.decode (BSC.pack "666F6F626172")

case_b16_dec_failure = do
    -- odd number of input bytes
    (Left (BSC.pack "fooba", BS.pack [55])) @=? (B16.decode $ BS.pack [54,54,54,70,54,70,54,50,54,49,55])

-- {{{1 base32
case_b32_enc_foobar = do
    BSC.empty                   @=? B32.encode BSC.empty
    BSC.pack "MY======"         @=? B32.encode (BSC.pack "f")
    BSC.pack "MZXQ===="         @=? B32.encode (BSC.pack "fo")
    BSC.pack "MZXW6==="         @=? B32.encode (BSC.pack "foo")
    BSC.pack "MZXW6YQ="         @=? B32.encode (BSC.pack "foob")
    BSC.pack "MZXW6YTB"         @=? B32.encode (BSC.pack "fooba")
    BSC.pack "MZXW6YTBOI======" @=? B32.encode (BSC.pack "foobar")

case_b32_dec_foobar = do
    Right BS.empty            @=? B32.decode BS.empty
    Right (BSC.pack "f")      @=? B32.decode (BSC.pack "MY======")
    Right (BSC.pack "fo")     @=? B32.decode (BSC.pack "MZXQ====")
    Right (BSC.pack "foo")    @=? B32.decode (BSC.pack "MZXW6===")
    Right (BSC.pack "foob")   @=? B32.decode (BSC.pack "MZXW6YQ=")
    Right (BSC.pack "fooba")  @=? B32.decode (BSC.pack "MZXW6YTB")
    Right (BSC.pack "foobar") @=? B32.decode (BSC.pack "MZXW6YTBOI======")

case_b32_dec_failures = do
    -- illegal char
    Left (BSC.empty, BSC.pack "M=XW6YTB") @=? (B32.b32_decode_part $ BSC.pack "M=XW6YTB")
    -- full block
    Nothing @=? (B32.b32_decode_final $ BSC.pack "MZXW6YTB")
    -- too short
    Nothing @=? (B32.b32_decode_final $ BSC.pack "MZXW6Y=")

-- {{{1 base32hex
case_b32h_enc_foobar = do
    BSC.empty                   @=? B32H.encode BS.empty
    BSC.pack "CO======"         @=? B32H.encode (BSC.pack "f")
    BSC.pack "CPNG===="         @=? B32H.encode (BSC.pack "fo")
    BSC.pack "CPNMU==="         @=? B32H.encode (BSC.pack "foo")
    BSC.pack "CPNMUOG="         @=? B32H.encode (BSC.pack "foob")
    BSC.pack "CPNMUOJ1"         @=? B32H.encode (BSC.pack "fooba")
    BSC.pack "CPNMUOJ1E8======" @=? B32H.encode (BSC.pack "foobar")

case_b32h_dec_foobar = do
    Right BS.empty            @=? B32H.decode BS.empty
    Right (BSC.pack "f") @=? B32H.decode (BSC.pack "CO======")
    Right (BSC.pack "fo") @=? B32H.decode (BSC.pack "CPNG====")
    Right (BSC.pack "foo") @=? B32H.decode (BSC.pack "CPNMU===")
    Right (BSC.pack "foob") @=? B32H.decode (BSC.pack "CPNMUOG=")
    Right (BSC.pack "fooba") @=? B32H.decode (BSC.pack "CPNMUOJ1")
    Right (BSC.pack "foobar") @=? B32H.decode (BSC.pack "CPNMUOJ1E8======")

case_b32h_dec_failures = do
    --  illegal char
    Left (BS.empty, BSC.pack "C=NMUOJ1") @=? (B32H.b32h_decode_part $ BSC.pack "C=NMUOJ1")
    -- full block
    Nothing @=? (B32H.b32h_decode_final $ BSC.pack "CPNMUOJ1")
    -- too short
    Nothing @=? (B32H.b32h_decode_final $ BSC.pack "CPNMUO=")

-- {{{1 base64
case_b64_enc_foobar = do
    BSC.empty           @=? B64.encode BSC.empty
    BSC.pack "Zg=="     @=? B64.encode (BSC.pack "f")
    BSC.pack "Zm8="     @=? B64.encode (BSC.pack "fo")
    BSC.pack "Zm9v"     @=? B64.encode (BSC.pack "foo")
    BSC.pack "Zm9vYg==" @=? B64.encode (BSC.pack "foob")
    BSC.pack "Zm9vYmE=" @=? B64.encode (BSC.pack "fooba")
    BSC.pack "Zm9vYmFy" @=? B64.encode (BSC.pack "foobar")

case_b64_enc_specials = do
    -- /++/
    BSC.pack "/++/" @=? B64.encode (BS.pack [255,239,191])

case_b64_dec_foobar = do
    Right BSC.empty           @=? B64.decode BSC.empty
    Right (BSC.pack "f")      @=? B64.decode (BSC.pack "Zg==")
    Right (BSC.pack "fo")     @=? B64.decode (BSC.pack "Zm8=")
    Right (BSC.pack "foo")    @=? B64.decode (BSC.pack "Zm9v")
    Right (BSC.pack "foob")   @=? B64.decode (BSC.pack "Zm9vYg==")
    Right (BSC.pack "fooba")  @=? B64.decode (BSC.pack "Zm9vYmE=")
    Right (BSC.pack "foobar") @=? B64.decode (BSC.pack "Zm9vYmFy")

case_b64_dec_specials = do
    -- /++/
    Right (BS.pack [255,239,191]) @=? B64.decode (BSC.pack "/++/")

-- {{{1 base64url
case_b64u_enc_foobar = do
    BSC.empty           @=? B64U.encode BSC.empty
    BSC.pack "Zg=="     @=? B64U.encode (BSC.pack "f")
    BSC.pack "Zm8="     @=? B64U.encode (BSC.pack "fo")
    BSC.pack "Zm9v"     @=? B64U.encode (BSC.pack "foo")
    BSC.pack "Zm9vYg==" @=? B64U.encode (BSC.pack "foob")
    BSC.pack "Zm9vYmE=" @=? B64U.encode (BSC.pack "fooba")
    BSC.pack "Zm9vYmFy" @=? B64U.encode (BSC.pack "foobar")

case_b64u_enc_specials = do
    -- _--_
    BSC.pack "_--_" @=? (B64U.encode $ BS.pack [255,239,191])

case_b64u_dec_foobar = do
    Right BSC.empty           @=? B64U.decode BSC.empty
    Right (BSC.pack "f")      @=? B64U.decode (BSC.pack "Zg==")
    Right (BSC.pack "fo")     @=? B64U.decode (BSC.pack "Zm8=")
    Right (BSC.pack "foo")    @=? B64U.decode (BSC.pack "Zm9v")
    Right (BSC.pack "foob")   @=? B64U.decode (BSC.pack "Zm9vYg==")
    Right (BSC.pack "fooba")  @=? B64U.decode (BSC.pack "Zm9vYmE=")
    Right (BSC.pack "foobar") @=? B64U.decode (BSC.pack "Zm9vYmFy")

case_b64u_dec_specials = do
    -- _--_
    Right (BS.pack [255,239,191]) @=? B64U.decode (BSC.pack "_--_")

-- {{{1 base85
case_b85_enc_foobar = do
    -- foobar
    BS.empty @=? B85.encode BS.empty
    BS.pack [65,99] @=? (B85.encode $ BS.pack [102])
    BS.pack [65,111,64] @=? (B85.encode $ BS.pack [102,111])
    BS.pack [65,111,68,83] @=? (B85.encode $ BS.pack [102,111,111])
    BS.pack [65,111,68,84,115] @=? (B85.encode $ BS.pack [102,111,111,98])
    BS.pack [65,111,68,84,115,64,47] @=? (B85.encode $ BS.pack [102,111,111,98,97])
    BS.pack [65,111,68,84,115,64,60,41] @=? (B85.encode $ BS.pack [102,111,111,98,97,114])

case_b85_enc_specials = do
    -- all zero
    BS.pack [122] @=? (B85.encode $ BS.pack [0,0,0,0])
    -- all space
    BS.pack [121] @=? (B85.encode $ BS.pack [32,32,32,32])
    -- double special
    BS.pack [121,122] @=? (B85.encode $ BS.pack [32,32,32,32,0,0,0,0])

case_b85_dec_foobar = do
    -- foobar
    Right BS.empty @=? B85.decode BS.empty
    (Right $ BS.pack [102]) @=? (B85.decode $ BS.pack [65,99])
    (Right $ BS.pack [102,111]) @=? (B85.decode $ BS.pack [65,111,64])
    (Right $ BS.pack [102,111,111]) @=? (B85.decode $ BS.pack [65,111,68,83])
    (Right $ BS.pack [102,111,111,98]) @=? (B85.decode $ BS.pack [65,111,68,84,115])
    (Right $ BS.pack [102,111,111,98,97]) @=? (B85.decode $ BS.pack [65,111,68,84,115,64,47])
    (Right $ BS.pack [102,111,111,98,97,114]) @=? (B85.decode $ BS.pack [65,111,68,84,115,64,60,41])

case_b85_dec_specials = do
    -- all zero
    (Right $ BS.pack [0,0,0,0]) @=? (B85.decode $ BS.pack [122])
    -- all space
    (Right $ BS.pack [32,32,32,32]) @=? (B85.decode $ BS.pack [121])
    -- double special
    (Right $ BS.pack [32,32,32,32,0,0,0,0]) @=? (B85.decode $ BS.pack [121,122])

-- {{{1 quoted printable
case_qp_enc_foobar = do
    BS.empty @=? QP.encode BS.empty
    BS.pack [102,111,111,98,97,114] @=? (QP.encode $ BS.pack [102,111,111,98,97,114])

case_qp_dec_foobar = do
    Right BS.empty @=? QP.decode BS.empty
    (Right $ BS.pack [102,111,111,98,97,114]) @=? (QP.decode $ BS.pack [102,111,111,98,97,114])

-- {{{1 uu
case_uu_enc_foobar = do
    BS.empty            @=? Uu.encode BS.empty
    BSC.pack "9@"       @=? Uu.encode (BSC.pack "f")
    BSC.pack "9F\\"     @=? Uu.encode (BSC.pack "fo")
    BSC.pack "9F]O"     @=? Uu.encode (BSC.pack "foo")
    BSC.pack "9F]O8@"   @=? Uu.encode (BSC.pack "foob")
    BSC.pack "9F]O8F$"  @=? Uu.encode (BSC.pack "fooba")
    BSC.pack "9F]O8F%R" @=? Uu.encode (BSC.pack "foobar")

case_uu_dec_foobar = do
    Right BS.empty            @=? Uu.decode BS.empty
    Right (BSC.pack "f")      @=? Uu.decode (BSC.pack "9@")
    Right (BSC.pack "fo")     @=? Uu.decode (BSC.pack "9F\\")
    Right (BSC.pack "foo")    @=? Uu.decode (BSC.pack "9F]O")
    Right (BSC.pack "foob")   @=? Uu.decode (BSC.pack "9F]O8@")
    Right (BSC.pack "fooba")  @=? Uu.decode (BSC.pack "9F]O8F$")
    Right (BSC.pack "foobar") @=? Uu.decode (BSC.pack "9F]O8F%R")

-- {{{1 xx
case_xx_enc_foobar = do
    -- foobar
    BS.empty @=? Xx.encode BS.empty
    BS.pack [78,85] @=? (Xx.encode $ BS.pack [102])
    BS.pack [78,97,119] @=? (Xx.encode $ BS.pack [102,111])
    BS.pack [78,97,120,106] @=? (Xx.encode $ BS.pack [102,111,111])
    BS.pack [78,97,120,106,77,85] @=? (Xx.encode $ BS.pack [102,111,111,98])
    BS.pack [78,97,120,106,77,97,50] @=? (Xx.encode $ BS.pack [102,111,111,98,97])
    BS.pack [78,97,120,106,77,97,51,109] @=? (Xx.encode $ BS.pack [102,111,111,98,97,114])

case_xx_dec_foobar = do
    -- foobar
    Right BS.empty @=? Xx.decode BS.empty
    (Right $ BS.pack [102]) @=? (Xx.decode $ BS.pack [78,85])
    (Right $ BS.pack [102,111]) @=? (Xx.decode $ BS.pack [78,97,119])
    (Right $ BS.pack [102,111,111]) @=? (Xx.decode $ BS.pack [78,97,120,106])
    (Right $ BS.pack [102,111,111,98]) @=? (Xx.decode $ BS.pack [78,97,120,106,77,85])
    (Right $ BS.pack [102,111,111,98,97]) @=? (Xx.decode $ BS.pack [78,97,120,106,77,97,50])
    (Right $ BS.pack [102,111,111,98,97,114]) @=? (Xx.decode $ BS.pack [78,97,120,106,77,97,51,109])

-- {{{1 yenc
case_y_enc_foobar = do
    -- foobar
    BS.empty @=? Y.encode BS.empty
    BS.pack [144] @=? (Y.encode $ BS.pack [102])
    BS.pack [144,153] @=? (Y.encode $ BS.pack [102,111])
    BS.pack [144,153,153] @=? (Y.encode $ BS.pack [102,111,111])
    BS.pack [144,153,153,140] @=? (Y.encode $ BS.pack [102,111,111,98])
    BS.pack [144,153,153,140,139] @=? (Y.encode $ BS.pack [102,111,111,98,97])
    BS.pack [144,153,153,140,139,156] @=? (Y.encode $ BS.pack [102,111,111,98,97,114])

case_y_enc_specials = do
    -- expanded chars
    BS.pack [61,64] @=? (Y.encode $ BS.pack [214])
    BS.pack [61,74] @=? (Y.encode $ BS.pack [224])
    BS.pack [61,77] @=? (Y.encode $ BS.pack [227])
    BS.pack [61,125] @=? (Y.encode $ BS.pack [19])

case_y_dec_foobar = do
    -- foobar
    Right BS.empty @=? Y.decode BS.empty
    (Right $ BS.pack [102]) @=? (Y.decode $ BS.pack [144])
    (Right $ BS.pack [102,111]) @=? (Y.decode $ BS.pack [144,153])
    (Right $ BS.pack [102,111,111]) @=? (Y.decode $ BS.pack [144,153,153])
    (Right $ BS.pack [102,111,111,98]) @=? (Y.decode $ BS.pack [144,153,153,140])
    (Right $ BS.pack [102,111,111,98,97]) @=? (Y.decode $ BS.pack [144,153,153,140,139])
    (Right $ BS.pack [102,111,111,98,97,114]) @=? (Y.decode $ BS.pack [144,153,153,140,139,156])

case_y_dec_specials = do
    -- expanded chars
    (Right $ BS.pack [214]) @=? (Y.decode $ BS.pack [61,64])
    (Right $ BS.pack [224]) @=? (Y.decode $ BS.pack [61,74])
    (Right $ BS.pack [227]) @=? (Y.decode $ BS.pack [61,77])
    (Right $ BS.pack [19]) @=? (Y.decode $ BS.pack [61,125])

-- {{{1 tests & main
tests = [$(testGroupGenerator)]

main = defaultMain tests
