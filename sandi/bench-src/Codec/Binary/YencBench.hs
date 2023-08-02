module Codec.Binary.YencBench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.Yenc

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = encode data1M
        enc10M = encode data10M
     in
        [ bench "enc yenc 1M" $ nf encode data1M
        , bench "dec yenc 1M" $ nf decode enc1M
        , bench "enc yenc 10M" $ nf encode data10M
        , bench "dec yenc 10M" $ nf decode enc10M
        ]
