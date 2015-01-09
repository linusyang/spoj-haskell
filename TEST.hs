-- Copyright (c) 2015 Linus Yang

module Main where

main :: IO ()
main = do
  s <- getLine
  let n = read s :: Int
  if n == 42 then return () else do
    print n
    main
