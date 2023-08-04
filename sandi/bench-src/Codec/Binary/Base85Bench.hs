{-# LANGUAGE ImportQualifiedPost #-}
module Codec.Binary.Base85Bench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.Base85 qualified as   B85

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = B85.encode data1M
        enc10M = B85.encode data10M
     in
        [ bench "enc base 85 1M" $ nf B85.encode data1M
        , bench "dec base 85 1M" $ nf B85.decode enc1M
        , bench "enc base 85 10M" $ nf B85.encode data10M
        , bench "dec base 85 10M" $ nf B85.decode enc10M
        ]
