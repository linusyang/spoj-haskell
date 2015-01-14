module Main where

import System.Random

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p:sieve [x | x <- xs, x `mod` p /= 0]

smallUpper :: Int
smallUpper = 100

mrIterNum :: Int
mrIterNum = 10

smallprimes :: [Int]
smallprimes = takeWhile (< smallUpper) primes

powmod :: Integer -> Integer -> Integer -> Integer
powmod n a b | b == 0 = 1
             | otherwise = (((c * x) `mod` n) * x) `mod` n
  where c = if b `mod` 2 == 0 then 1 else a
        x = powmod n a (b `div` 2)

powtwo :: Integer -> (Int, Integer)
powtwo x | x `mod` 2 == 1 = (0, x)
         | otherwise = (n + 1, y)
  where (n, y) = powtwo (x `div` 2)

sqrtest :: Int -> Integer -> Integer -> Bool
sqrtest 0 _ _ = False
sqrtest s x n | x' == 1 = False
              | x' == n - 1 = True
              | otherwise = sqrtest (s - 1) x' n
  where x' = (x * x) `mod` n

maybeprime :: Integer -> Integer -> Bool
maybeprime n a | x == 1 || x == n - 1 = True
               | otherwise = sqrtest (s - 1) x n
  where (s, d) = powtwo $ n - 1
        x = powmod n a d

witnesses :: Int -> Integer -> IO [Integer]
witnesses k n 
  | n < 9080191         = return [31,73]
  | n < 4759123141      = return [2,7,61]
  | n < 3474749660383   = return [2,3,5,7,11,13]
  | n < 341550071728321 = return [2,3,5,7,11,13,17]
  | otherwise           = do g <- newStdGen
                             let ws = take k (randomRs (3,n-1) g)
                             return $ map (\x -> if x `mod` 2 == 0 then x - 1 else x) ws

isPrime :: Integer -> IO Bool
isPrime n | n < 1 = return False
          | n == 2 = return True
          | n `mod` 2 == 0 = return False
          | n < toInteger smallUpper = return $ (fromInteger n) `elem` smallprimes
          | otherwise = do
              ws <- witnesses mrIterNum n
              return . and $ map (maybeprime n) ws

loop :: Int -> IO ()
loop 0 = return ()
loop n = do
  inp <- getLine
  let x = read inp :: Integer
  p <- isPrime x
  putStrLn $ if p then "YES" else "NO"
  loop (n - 1)

main :: IO ()
main = do
  inp <- getLine
  let n = read inp :: Int
  loop n
