{-# LANGUAGE ImportQualifiedPost #-}

module Codec.Binary.YencBench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.Yenc qualified as Yenc

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = Yenc.encode data1M
        enc10M = Yenc.encode data10M
     in
        [ bench "enc yenc 1M" $ nf Yenc.encode data1M
        , bench "dec yenc 1M" $ nf Yenc.decode enc1M
        , bench "enc yenc 10M" $ nf Yenc.encode data10M
        , bench "dec yenc 10M" $ nf Yenc.decode enc10M
        ]
