-- {-# LANGUAGE Strict #-}

module Main where

import           Criterion.Main        (bench, bgroup, defaultMain, nfIO)
import           Data.ByteString       (ByteString, hGetSome)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import           Data.Int              (Int64)
import           Data.Word             (Word8)
import           Foreign.Marshal.Alloc (alloca, allocaBytes)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (peek)
import           System.IO             (FilePath, Handle, IOMode (ReadMode),
                                        SeekMode (AbsoluteSeek), hGetBuf, hSeek,
                                        openBinaryFile)
-- import           System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import System.IO.MMap

openRead :: FilePath -> IO Handle
openRead path =
  openBinaryFile path ReadMode

absoluteSeek :: Handle -> Integer -> IO ()
absoluteSeek handle =
  hSeek handle AbsoluteSeek

readBytesInternal :: Handle -> Int -> Ptr Word8 -> IO (Maybe Word8)
readBytesInternal handle numBytes ptr = do
  res <- hGetBuf handle ptr numBytes
  if res > 0
    then Just <$> peek ptr
    else return Nothing

readBytes :: Handle -> Int -> IO (Maybe Word8)
readBytes handle numBytes =
  allocaBytes numBytes $ readBytesInternal handle numBytes

readBytesAtPosition :: Handle -> Int -> Integer -> IO (Maybe Word8)
readBytesAtPosition handle numBytes position = do
  absoluteSeek handle position
  readBytes handle numBytes

readByteStringAtPosition :: Handle -> Int -> Integer -> IO ByteString
readByteStringAtPosition handle numBytes position = do
  absoluteSeek handle position
  hGetSome handle numBytes

testReadBytes :: Integer -> IO ()
testReadBytes n = do
  handle <- openRead "data.txt"
  let list = fmap (readBytesAtPosition handle 128 . (100 *)) [0 .. n]
  sequence_ list

testByteString :: Integer -> IO ()
testByteString n = do
  handle <- openRead "data.txt"
  let list = fmap (readByteStringAtPosition handle 128 . (100 *)) [0 .. n]
  sequence_ list

testByteStringLoop :: Int -> IO ()
testByteStringLoop n = do
  handle <- openRead "data.txt"
  go n (fromIntegral n) 0 handle
  where
    go n max count handle
      | max == count = return ()
      | otherwise = do
          _ <- readByteStringAtPosition handle 128 (100 * count)
          go n max (count + 1) handle


-- mmapFileByteString

readMappedBytesAtPosition :: ByteString -> Int -> Int -> IO ByteString
readMappedBytesAtPosition bs start end =
  return $ BS.take (end - start) $ BS.drop start bs 

readMappedBytesAtPositionL :: BSL.ByteString -> Int64 -> Int64 -> IO BSL.ByteString
readMappedBytesAtPositionL bs start end =
  return $ BSL.take (end - start) $ BSL.drop start bs 

testReadByteStringMMap :: Int -> IO ()
testReadByteStringMMap n = do
  byteString <- mmapFileByteString "data.txt" Nothing
  let list = fmap (readMappedBytesAtPosition byteString 128 . (100 *)) [0 .. n]
  sequence_ list

testReadByteStringTD :: Int -> IO ()
testReadByteStringTD n = do
  byteString <- BS.readFile "data.txt"
  let list = fmap (readMappedBytesAtPosition byteString 128 . (100 *)) [0 .. n]
  sequence_ list

-- testReadByteStringTDL :: Int64 -> IO [BSL.ByteString]
-- testReadByteStringTDL n = do
--   byteString <- BSL.readFile "data.txt"
--   let list = fmap (readMappedBytesAtPositionL byteString 128 . (100 *)) [0 .. n]
--   sequence list

testReadByteStringTDL :: Int64 -> IO Word8
testReadByteStringTDL n = do
  byteString <- BSL.readFile "data.txt"
  lst <- traverse (readMappedBytesAtPositionL byteString 128 . (1 *)) [0 .. n]
  -- sum (fmap BSL.head lst)

main :: IO ()
main = defaultMain [
        bgroup "seek"
        [ bench "System.IO.hGetBuf" $ nfIO (testReadBytes 100000)
        -- , bench "Data.ByteString.hGetSome" $ nfIO (testByteString 100000)
        -- , bench "Data.ByteString.hGetSome Loop" $ nfIO (testByteStringLoop 100000)
        -- , bench "Data.ByteString mmap'd" $ nfIO (testReadByteStringMMap 100000)
        , bench "Data.ByteString TD" $ nfIO (testReadByteStringTD 100000)
        , bench "Data.ByteString TDL" $ nfIO (testReadByteStringTDL 100000)

        ]
    ]

{- Results with -O2

benchmarking seek/System.IO.hGetBuf
time                 171.7 ms   (170.4 ms .. 172.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 170.9 ms   (170.5 ms .. 171.3 ms)
std dev              559.4 μs   (368.6 μs .. 836.6 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking seek/Data.ByteString.hGetSome
time                 174.4 ms   (173.4 ms .. 176.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 173.8 ms   (173.2 ms .. 174.5 ms)
std dev              859.2 μs   (542.8 μs .. 1.245 ms)
variance introduced by outliers: 12% (moderately inflated)

-}
