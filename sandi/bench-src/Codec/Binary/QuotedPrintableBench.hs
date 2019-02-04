module Codec.Binary.QuotedPrintableBench where

import Criterion.Main (bench, nf)

import Codec.Binary.QuotedPrintable

mkBenchs data1M data10M = let
        enc1M = encode data1M
        enc10M = encode data10M
    in
        [ bench "enc quoted printable 1M" $ nf encode data1M
        , bench "dec quoted printable 1M" $ nf decode enc1M
        , bench "enc quoted printable 10M" $ nf encode data10M
        , bench "dec quoted printable 10M" $ nf decode enc10M
        ]
