module Main where

import Data.Char (ord)

type Stat = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

str2stat :: String -> Stat
str2stat xs = (a0, a1, a2, a3, a4, a5, a6, a7, a8)
  where (a0:a1:a2:a3:a4:a5:a6:a7:a8:_) = map (\x -> ord x - ord '0') xs

stat2int :: Stat -> [Int]
stat2int (a0, a1, a2, a3, a4, a5, a6, a7, a8) = a0:a1:a2:a3:a4:a5:a6:a7:a8:[]

rotateRight :: Int -> Stat -> Stat
rotateRight n (a0, a1, a2, a3, a4, a5, a6, a7, a8)
  | n == 0 = (a3, a0, a2, a4, a1, a5, a6, a7, a8)
  | n == 1 = (a0, a4, a1, a3, a5, a2, a6, a7, a8)
  | n == 2 = (a0, a1, a2, a6, a3, a5, a7, a4, a8)
  | otherwise = (a0, a1, a2, a3, a7, a4, a6, a8, a5)

rotateLeft :: Int -> Stat -> Stat
rotateLeft n (a0, a1, a2, a3, a4, a5, a6, a7, a8)
  | n == 0 = (a1, a4, a2, a0, a3, a5, a6, a7, a8)
  | n == 1 = (a0, a2, a5, a3, a1, a4, a6, a7, a8)
  | n == 2 = (a0, a1, a2, a4, a7, a5, a3, a6, a8)
  | otherwise = (a0, a1, a2, a3, a5, a8, a6, a4, a7)

rotates :: Stat -> [(Int, Stat)]
rotates s = foldr (\x v -> (x, rotateRight x s) : (x + 4, rotateLeft x s) : v) [] [0..3]

distance :: Stat -> Int
distance st = sum $ zipWith manhatten [0..8] (stat2int st)
  where manhatten p x = abs (p `div` 3 - pred x `div` 3) +
                        abs (p `mod` 3 - pred x `mod` 3)

heuristic :: Stat -> Int
heuristic st = (distance st + 3) `div` 4

idastar :: Stat -> Int -> Int -> Bool
idastar st bound pre
  | dist > bound = False
  | dist == 0 && bound == 0 = True
  | otherwise = any (\(st', bound', pre') -> idastar st' bound' pre') children
  where dist = heuristic st
        nextstats = filter (\(x, _) -> pre == -1 || x - pre /= 4) $ rotates st
        children = map (\(pre', st') -> (st', pred bound, pre')) nextstats

solve :: Int -> Stat -> Int -> Int
solve limit start depth
  | depth > limit = -1
  | idastar start depth (-1) = depth
  | otherwise = solve limit start (succ depth)

loop :: Int -> IO ()
loop n = do
  inp <- getLine
  if inp == "0000000000" then return () else do
    let limit = ord (head inp) - ord '0'
        start = str2stat $ tail inp
        depth = heuristic start
    putStrLn $ show n ++ ". " ++ show (solve limit start depth)
    loop (n + 1)

main :: IO ()
main = do
    loop 1
