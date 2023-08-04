{-# LANGUAGE ImportQualifiedPost #-}
module Codec.Binary.Base32Bench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.Base32 qualified as B32

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = B32.encode data1M
        enc10M = B32.encode data10M
     in
        [ bench "enc base 32 1M" $ nf B32.encode data1M
        , bench "dec base 32 1M" $ nf B32.decode enc1M
        , bench "enc base 32 10M" $ nf B32.encode data10M
        , bench "dec base 32 10M" $ nf B32.decode enc10M
        ]
