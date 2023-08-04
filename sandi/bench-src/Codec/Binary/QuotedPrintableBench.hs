{-# LANGUAGE ImportQualifiedPost #-}

module Codec.Binary.QuotedPrintableBench where

import Criterion.Main (Benchmark, bench, nf)
import Data.ByteString (ByteString)

import Codec.Binary.QuotedPrintable qualified as QP

mkBenchs :: ByteString -> ByteString -> [Benchmark]
mkBenchs data1M data10M =
    let
        enc1M = QP.encode data1M
        enc10M = QP.encode data10M
     in
        [ bench "enc quoted printable 1M" $ nf QP.encode data1M
        , bench "dec quoted printable 1M" $ nf QP.decode enc1M
        , bench "enc quoted printable 10M" $ nf QP.encode data10M
        , bench "dec quoted printable 10M" $ nf QP.decode enc10M
        ]
