-- Copyright (c) 2015 Linus Yang

module Main where
import Data.Char (ord, chr)
import qualified Data.Sequence as S
import Data.Foldable (toList)

process :: S.Seq Int -> Int -> Int -> S.Seq Int
process xs pl pr
  | pl < 0 = (S.<|) 1 (S.update (S.length xs - 1) 1 $ xs)
  | xl < 9 = let xl' = xl + 1 in S.update pl xl' . S.update pr xl' $ xs
  | otherwise = process (S.update pl 0 . S.update pr 0 $ xs) (pl - 1) (pr + 1)
  where xl = S.index xs pl

getPos :: Int -> (Int, Int)
getPos n | n `mod` 2 == 0 = (mid -1, mid)
         | otherwise = (mid, mid)
  where mid = n `div` 2

palinize :: [Int] -> [Int]
palinize xs = left ++ mid ++ right
  where len = length xs
        (left, right') = splitAt (len `div` 2) xs
        mid = if len `mod` 2 == 0 then [] else [head right']
        right = reverse left

greaterThan :: [Int] -> [Int] -> Bool
greaterThan [] [] = False
greaterThan (x:xs) (y:ys) | x > y = True
                          | x < y = False
                          | otherwise = greaterThan xs ys

needProcess :: [Int] -> Bool
needProcess xs = not $ greaterThan (reverse left) right
  where len = length xs
        (left, right') = splitAt (len `div` 2) xs
        right = if len `mod` 2 == 0 then right' else tail right'

next :: [Int] -> [Int]
next xs = if needProcess xs then
            palinize . toList $ process (S.fromList xs) pl pr
          else palinize xs
  where (pl, pr) = getPos $ length xs

printIntList :: [Int] -> IO ()
printIntList = mapM_ (\x -> putChar $ chr (x + ord '0'))

readIntList :: IO [Int]
readIntList = do
  xs <- getLine -- EOF will kill you if using getChar!
  return $ map (\x -> ord x - ord '0') xs

readLoop :: Int -> IO ()
readLoop 0 = return ()
readLoop n = do
  xs <- readIntList
  printIntList . next $ xs
  putChar '\n'
  readLoop (n - 1)

main :: IO ()
main = do
  inp <- getLine
  let n = read inp :: Int
  readLoop n
