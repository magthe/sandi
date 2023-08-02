{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Criterion.Main (defaultMain)
import Data.ByteString qualified as BS
import System.IO

import Codec.Binary.Base16Bench qualified as B16B
import Codec.Binary.Base32Bench qualified as B32B
import Codec.Binary.Base32HexBench qualified as B32HB
import Codec.Binary.Base64Bench qualified as B64B
import Codec.Binary.Base64UrlBench qualified as B64UB
import Codec.Binary.Base85Bench qualified as B85B
import Codec.Binary.QuotedPrintableBench qualified as QPB
import Codec.Binary.UuBench qualified as UuB
import Codec.Binary.XxBench qualified as XxB
import Codec.Binary.YencBench qualified as YB

main :: IO ()
main = do
    h <- openFile "/dev/urandom" ReadMode
    data1M <- BS.hGet h (1024 * 1024)
    data10M <- BS.hGet h (10 * 1024 * 1024)
    defaultMain $
        B16B.mkBenchs data1M data10M
            ++ B32B.mkBenchs data1M data10M
            ++ B32HB.mkBenchs data1M data10M
            ++ B64B.mkBenchs data1M data10M
            ++ B64UB.mkBenchs data1M data10M
            ++ B85B.mkBenchs data1M data10M
            ++ QPB.mkBenchs data1M data10M
            ++ UuB.mkBenchs data1M data10M
            ++ XxB.mkBenchs data1M data10M
            ++ YB.mkBenchs data1M data10M
