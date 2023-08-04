{-# LANGUAGE ImportQualifiedPost #-}
module Codec.Binary.Base64Bench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.Base64 qualified as B64

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = B64.encode data1M
        enc10M = B64.encode data10M
     in
        [ bench "enc base 64 1M" $ nf B64.encode data1M
        , bench "dec base 64 1M" $ nf B64.decode enc1M
        , bench "enc base 64 10M" $ nf B64.encode data10M
        , bench "dec base 64 10M" $ nf B64.decode enc10M
        ]
