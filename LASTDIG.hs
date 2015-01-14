module Main where
digits n = n : (map (\x -> (x * n) `mod` 10) $ digits n)
rotate xs = last xs : init xs
findLoop n = rotate $ n' : (takeWhile (/= n') . tail $ digits n) where n' = n `mod` 10
powlast a b = aloop !! (b `mod` len)
  where aloop = findLoop $ a `mod` 10
        len = length aloop
loop :: Int -> IO ()
loop 0 = return ()
loop n = do
  inp <- getLine
  let inps = words inp
      a = read . head $ inps :: Int
      b = read . last $ inps :: Int
  print $ if b == 0 then 1 else powlast a b
  loop (n - 1)
main :: IO ()
main = do
  x <- getLine
  let n = read x :: Int
  loop n
