{-# LANGUAGE ImportQualifiedPost #-}

module Codec.Binary.UuBench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.Uu qualified as Uu

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = Uu.encode data1M
        enc10M = Uu.encode data10M
     in
        [ bench "enc uu 1M" $ nf Uu.encode data1M
        , bench "dec uu 1M" $ nf Uu.decode enc1M
        , bench "enc uu 10M" $ nf Uu.encode data10M
        , bench "dec uu 10M" $ nf Uu.decode enc10M
        ]
