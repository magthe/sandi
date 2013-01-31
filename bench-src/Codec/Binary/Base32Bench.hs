module Codec.Binary.Base32Bench where

import Criterion.Main (bench, nf)

import Codec.Binary.Base32

mkBenchs data1M data10M = let
        enc1M = encode data1M
        enc10M = encode data10M
    in
        [ bench "enc base 32 1M" $ nf encode data1M
        , bench "dec base 32 1M" $ nf decode enc1M
        , bench "enc base 32 10M" $ nf encode data10M
        , bench "dec base 32 10M" $ nf decode enc10M
        ]
