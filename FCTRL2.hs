-- Copyright (c) 2015 Linus Yang

module Main where

loop :: Int -> IO ()
loop 0 = return ()
loop n = do
  inp <- getLine
  let x' = read inp :: Integer
      x = if x' < 1 then 1 else x'
  print $ product [1..x]
  loop (n - 1)

main :: IO ()
main = do
  inp <- getLine
  let n = read inp :: Int
  loop n
