-- Copyright (c) 2015 Linus Yang

module Main where

divList :: Int -> Int -> [Int]
divList 0 _ = []
divList x d = r:divList r d where r = x `div` d

calcWeight :: [Int] -> Int
calcWeight [] = 0
calcWeight xs@(_:xs') = sum . zipWith (*) [1..n] $ zipWith (-) xs xs'
  where n = length xs'

factor :: Int -> Int
factor n = calcWeight $ divList n 5

loopFactor :: Int -> IO ()
loopFactor 0 = return ()
loopFactor n = do
  inp <- getLine
  let x = read inp :: Int
  print $ factor x
  loopFactor (n - 1)

main :: IO ()
main = do
  inp <- getLine
  let n = read inp :: Int
  loopFactor n
