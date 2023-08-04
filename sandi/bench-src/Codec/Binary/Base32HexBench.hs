{-# LANGUAGE ImportQualifiedPost #-}

module Codec.Binary.Base32HexBench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.Base32Hex qualified as B32H

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = B32H.encode data1M
        enc10M = B32H.encode data10M
     in
        [ bench "enc base 32 hex 1M" $ nf B32H.encode data1M
        , bench "dec base 32 hex 1M" $ nf B32H.decode enc1M
        , bench "enc base 32 hex 10M" $ nf B32H.encode data10M
        , bench "dec base 32 hex 10M" $ nf B32H.decode enc10M
        ]
