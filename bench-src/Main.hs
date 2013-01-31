module Main where

import Criterion.Main (bench, defaultMain, nf)
import qualified Data.ByteString as BS
import System.IO

import qualified Codec.Binary.Base16Bench as B16B
import qualified Codec.Binary.Base32Bench as B32B
import qualified Codec.Binary.Base32HexBench as B32HB
import qualified Codec.Binary.Base64Bench as B64B
import qualified Codec.Binary.Base64UrlBench as B64UB
import qualified Codec.Binary.Base85Bench as B85B
import qualified Codec.Binary.QuotedPrintableBench as QPB
import qualified Codec.Binary.UuBench as UuB
import qualified Codec.Binary.XxBench as XxB
import qualified Codec.Binary.YencBench as YB

main :: IO ()
main = do
    h <- openFile "/dev/urandom" ReadMode
    data1M <- BS.hGet h (1024 * 1024)
    data10M <- BS.hGet h (10 * 1024 * 1024)
    defaultMain $ B16B.mkBenchs data1M data10M ++
        B32B.mkBenchs data1M data10M ++
        B32HB.mkBenchs data1M data10M ++
        B64B.mkBenchs data1M data10M ++
        B64UB.mkBenchs data1M data10M ++
        B85B.mkBenchs data1M data10M ++
        QPB.mkBenchs data1M data10M ++
        UuB.mkBenchs data1M data10M ++
        XxB.mkBenchs data1M data10M ++
        YB.mkBenchs data1M data10M
