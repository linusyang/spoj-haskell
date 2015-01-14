-- Copyright (c) 2015 Linus Yang

module Main where
import System.IO (isEOF)
import Data.Array.IO
import Data.Int (Int64)

type Int64Array = IOUArray Int64 Int64

copyArray :: Int64Array -> Int64Array -> Int64 -> Int64 -> Int64 -> IO Int64
copyArray dest src destIndex srcIndex n
  | n <= 0 = return destIndex
  | otherwise = do
      x <- readArray src srcIndex
      writeArray dest destIndex x
      copyArray dest src (succ destIndex) (succ srcIndex) (pred n)

mergeBack :: Int64 -> Int64 -> Int64 -> Int64 -> (Int64Array, Int64) -> (Int64Array, Int64) -> Int64Array -> IO (Int64, Int64, Int64, Int64)
mergeBack v i l r (leftArr, leftLen) (rightArr, rightLen) arr
  | l >= leftLen || r >= rightLen = return (v, i, l ,r)
  | otherwise = do
      x <- readArray leftArr l
      y <- readArray rightArr r
      if x <= y then do
        writeArray arr i x
        mergeBack v (succ i) (succ l) r (leftArr, leftLen) (rightArr, rightLen) arr
        else do
        writeArray arr i y
        mergeBack (v + leftLen - l) (succ i) l (succ r) (leftArr, leftLen) (rightArr, rightLen) arr

merge :: Int64Array -> Int64 -> Int64 -> Int64 -> IO Int64
merge arr left mid right = do
  let leftLen = mid - left
      rightLen = right - mid
  leftArr <- newArray (0, leftLen - 1) 0 :: IO (Int64Array)
  rightArr <- newArray (0, rightLen - 1) 0 :: IO (Int64Array)
  _ <- copyArray leftArr arr 0 left leftLen
  _ <- copyArray rightArr arr 0 mid rightLen
  (v, i, l, r) <- mergeBack 0 left 0 0 (leftArr, leftLen) (rightArr, rightLen) arr
  i' <- copyArray arr leftArr i l (leftLen - l)
  _ <- copyArray arr rightArr i' r (rightLen - r)
  return v
  
mergeSort :: Int64Array -> Int64 -> Int64 -> IO Int64
mergeSort arr left right
  | right - left <= 1 = return 0
  | otherwise = do
      let mid = (left + right) `div` 2
      l <- mergeSort arr left mid
      r <- mergeSort arr mid right
      m <- merge arr left mid right
      return $ l + r + m

readInt64Array :: Int64 -> Int64 -> Int64Array -> IO ()
readInt64Array i n arr
  | i >= n = do
      done <- isEOF
      if done then return () else do
        _ <- getLine
        return ()
  | otherwise = do
      x <- getLine
      writeArray arr i (read x :: Int64)
      readInt64Array (i + 1) n arr

loop :: Int64 -> IO ()
loop 0 = return ()
loop n = do
  inp <- getLine
  let arrlen = read inp :: Int64
  arr <- newArray (0, arrlen - 1) 0 :: IO (Int64Array)
  readInt64Array 0 arrlen arr
  x <- mergeSort arr 0 arrlen
  print x
  loop (n - 1)

main :: IO ()
main = do
  inp <- getLine
  _ <- getLine
  loop (read inp :: Int64)
