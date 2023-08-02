module Codec.Binary.Base85Bench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.Base85

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = encode data1M
        enc10M = encode data10M
     in
        [ bench "enc base 85 1M" $ nf encode data1M
        , bench "dec base 85 1M" $ nf decode enc1M
        , bench "enc base 85 10M" $ nf encode data10M
        , bench "dec base 85 10M" $ nf decode enc10M
        ]
