{-# LANGUAGE ImportQualifiedPost #-}
module Codec.Binary.Base64UrlBench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.Base64Url qualified as B64U

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = B64U.encode data1M
        enc10M = B64U.encode data10M
     in
        [ bench "enc base 64 url 1M" $ nf B64U.encode data1M
        , bench "dec base 64 url 1M" $ nf B64U.decode enc1M
        , bench "enc base 64 url 10M" $ nf B64U.encode data10M
        , bench "dec base 64 url 10M" $ nf B64U.decode enc10M
        ]
