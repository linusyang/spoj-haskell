-- Copyright (c) 2015 Linus Yang

module Main where
import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.List (isInfixOf)
import System.IO (isEOF)

isCStyle :: String -> Bool
isCStyle xs = all (\x -> isLower x || x == '_') xs &&
              not ("__" `isInfixOf` xs) &&
              head xs /= '_' &&
              last xs /= '_'

isJavaStyle :: String -> Bool
isJavaStyle xs = all (\x -> isLower x || isUpper x) xs &&
                 isLower (head xs)

toCStyle :: String -> String
toCStyle = foldr (\x v -> if isUpper x then '_':(toLower x):v else x:v) []

toJavaStyle :: String -> String
toJavaStyle [] = []
toJavaStyle [a] = [a]
toJavaStyle (x:y:xs) | x == '_' = toUpper y : toJavaStyle xs
                     | otherwise = x:toJavaStyle (y:xs)

trans :: String -> String
trans xs | isCStyle xs = toJavaStyle xs
         | isJavaStyle xs = toCStyle xs
         | otherwise = "Error!"

main :: IO ()
main = do
  done <- isEOF
  if done then return () else do
    inp <- getLine
    putStrLn $ trans inp
    main
