module Codec.Binary.UuBench where

import Criterion.Main (bench, nf)

import Codec.Binary.Uu

mkBenchs data1M data10M = let
        enc1M = encode data1M
        enc10M = encode data10M
    in
        [ bench "enc uu 1M" $ nf encode data1M
        , bench "dec uu 1M" $ nf decode enc1M
        , bench "enc uu 10M" $ nf encode data10M
        , bench "dec uu 10M" $ nf decode enc10M
        ]
