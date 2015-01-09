module Main where

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p:sieve [x | x <- xs, x `mod` p /= 0]

primesBelow :: Int -> [Int]
primesBelow n = takeWhile (<= n) primes

cachedUpper :: Int
cachedUpper = 32000

cachedPrimes :: [Int]
cachedPrimes = primesBelow cachedUpper

sieveWith :: [Int] -> [Int] -> [Int]
sieveWith [] xs = xs
sieveWith (p:ps) xs = sieveWith ps $ filter (\x -> x `mod` p /= 0 || x == p) xs

takeBetween :: Int -> Int -> [Int] -> [Int]
takeBetween x y = takeWhile (<= y) . dropWhile (< x)

primesBetween :: Int -> Int -> [Int]
primesBetween x y | x > y = []
                  | y <= cachedUpper = takeBetween x y cachedPrimes
                  | otherwise = sieveWith (takeWhile (<= (isqrt y + 1)) cachedPrimes) [x..y]

isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

printPrimePairs :: [(Int, Int)] -> IO ()
printPrimePairs p = do
 mapM_ (\(a, b) -> do
           mapM_ print $ primesBetween a b
           putChar '\n'
       ) $ init p
 mapM_ print $ primesBetween x y where (x, y) = last p

readPairs :: Int -> IO [(Int, Int)]
readPairs 0 = return []
readPairs n = do
  s <- getLine
  let list = words s
      x' = read (list !! 0) :: Int
      x = if x' < 2 then 2 else x'
      y = read (list !! 1) :: Int
  ps <- readPairs (n - 1)
  return $ (x, y) : ps

main :: IO ()
main = do
  s <- getLine
  let n = read s :: Int
  p <- readPairs n
  printPrimePairs p
