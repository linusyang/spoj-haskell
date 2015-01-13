module Main where
import Data.Char (ord, chr)

str2list :: String -> [Int]
str2list = foldr (\x v -> ord x - ord '0':v) []

listadd :: Int -> [Int] -> [Int] -> [Int]
listadd c [] [] = if c > 0 then [c] else []
listadd c (x:xs) [] = (c + x) `mod` 10 : listadd ((c + x) `div` 10) xs []
listadd c [] (y:ys) = (c + y) `mod` 10 : listadd ((c + y) `div` 10) [] ys
listadd c (x:xs) (y:ys) = (c + x + y) `mod` 10 : listadd ((c + x + y) `div` 10) xs ys

list2str :: [Int] -> String
list2str = foldr (\x v -> (chr $ x + ord '0'):v) [] . dropWhile (== 0)

loop :: Int -> IO ()
loop 0 = return ()
loop n = do
  inp <- getLine
  let inps = words inp
  putStrLn . list2str $ listadd 0 (str2list $ head inps) (str2list $ last inps)
  loop (n - 1)

main :: IO ()
main = do
  inp <- getLine
  let n = read inp :: Int
  loop n
