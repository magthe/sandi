module Codec.Binary.Base16Bench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.Base16

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = encode data1M
        enc10M = encode data10M
     in
        [ bench "enc base 16 1M" $ nf encode data1M
        , bench "dec base 16 1M" $ nf decode enc1M
        , bench "enc base 16 10M" $ nf encode data10M
        , bench "dec base 16 10M" $ nf decode enc10M
        ]
