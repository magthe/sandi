{-# LANGUAGE ImportQualifiedPost #-}
module Codec.Binary.XxBench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.Xx qualified as Xx

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = Xx.encode data1M
        enc10M = Xx.encode data10M
     in
        [ bench "enc xx 1M" $ nf Xx.encode data1M
        , bench "dec xx 1M" $ nf Xx.decode enc1M
        , bench "enc xx 10M" $ nf Xx.encode data10M
        , bench "dec xx 10M" $ nf Xx.decode enc10M
        ]
