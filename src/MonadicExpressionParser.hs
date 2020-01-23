module MonadicExpressionParser (eval) where

import Data.Char
import Control.Applicative

  {-
     Implementation of the basic parser - based on Graham Hutton's Monadic Parser in the book, 
     "Programming in Haskell" (2nd Edition).
     -}

newtype Parser a = P(String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> [(f v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P (\inp -> case parse pf inp of
                           [] -> []
                           [(f, out)] -> parse (fmap f px) out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                         [] -> parse q inp
                         [(v, out)] -> [(v, out)])

-- basic parsers

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x : xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = return []
string (c : cs) = do char c
                     string cs
                     return (c : cs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

digit :: Parser Char
digit = sat isDigit

nat :: Parser Int
nat = do n <- some digit
         return (read n)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
         <|> nat

token :: Parser a -> Parser a
token p = do space
             e <- p
             space
             return e

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol cs = token (string cs)


  {-
    EBNF Grammar for expressions:

      expr ::= term (+ expr | - expr | epsilon)

      term ::= pow (* term | / term | epsilon)

      pow ::= factor (^ pow | epsilon)

      factor ::= ( expr ) | integer

      integer ::= digit digit*

      digit ::= [0-9]

  -}

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             <|> do symbol "-"
                    e <- expr
                    return (t - e)
             <|> return t

term :: Parser Int
term = do p <- pow
          do symbol "*"
             t <- term
             return (p * t)
             <|> do symbol "/"
                    t <- term
                    return (p `div` t)
             <|> return p

pow :: Parser Int
pow = do f <- factor
         do symbol "^"
            p <- pow
            return (f ^ p)
            <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> integer

eval :: String -> Int
eval inp = case parse expr inp of
             [(n, [])] -> n
             [(_, out)] -> error $ "Unused input: " ++ out
             [] -> error "Invalid input"