module Codec.Binary.XxBench where

import Criterion.Main (bench, nf)

import Codec.Binary.Xx

mkBenchs data1M data10M = let
        enc1M = encode data1M
        enc10M = encode data10M
    in
        [ bench "enc xx 1M" $ nf encode data1M
        , bench "dec xx 1M" $ nf decode enc1M
        , bench "enc xx 10M" $ nf encode data10M
        , bench "dec xx 10M" $ nf decode enc10M
        ]
