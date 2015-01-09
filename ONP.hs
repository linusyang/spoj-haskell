module Main where
import Data.Char (isLower)

type RealParser a = String -> [(a, String)]
newtype Parser a = P (RealParser a)

data Expr = Nil
          | Var Char
          | Add Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Power Expr Expr

instance Show Expr where
  show Nil = []
  show (Var x) = [x]
  show (Add l r) = show l ++ show r ++ "+"
  show (Minus l r) = show l ++ show r ++ "-"
  show (Mult l r) = show l ++ show r ++ "*"
  show (Div l r) = show l ++ show r ++ "/"
  show (Power l r) = show l ++ show r ++ "^"

parse :: Parser a -> RealParser a
parse (P rp) = rp

instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  p >>= f = P (\inp -> case parse p inp of
                        [] -> []
                        [(v, out)] -> parse (f v) out)

failure :: Parser a
failure = P (\_ -> [])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
  [] -> parse q inp
  [(v, out)] -> [(v, out)])

item :: Parser Char
item = P (\inp -> case inp of
                   [] -> []
                   x:xs -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x
    else failure

var :: Parser Char
var = sat isLower

symbol :: Char -> Parser Char
symbol x = sat (== x)

expr :: Parser Expr
expr = do
  t <- term
  do
    symbol '+'
    e <- expr
    return $ Add t e
    +++ do
      symbol '-'
      e <- expr
      return $ Minus t e
    +++ return t

term :: Parser Expr
term = do
  p <- power
  do
    symbol '*'
    t <- term
    return $ Mult p t
    +++ do
      symbol '/'
      t <- term
      return $ Div p t
    +++ return p

power :: Parser Expr
power = do
  f <- factor
  do
    symbol '^'
    p <- power
    return $ Power f p
    +++ return f

factor :: Parser Expr
factor = do
  do
    symbol '('
    e <- expr
    symbol ')'
    return e
  +++ do
    v <- var
    return $ Var v

transform :: String -> Expr
transform xs = if null results then Nil
               else fst $ head results
  where results = parse expr xs

transInput :: Int -> IO ()
transInput 0 = return ()
transInput n = do
  xs <- getLine
  print $ transform xs
  transInput (n - 1)

main :: IO ()
main = do
  input <- getLine
  let n = read input :: Int
  transInput n
