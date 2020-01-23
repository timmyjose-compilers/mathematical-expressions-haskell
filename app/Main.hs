module Main where

import MonadicExpressionParser (eval)

main :: IO ()
main = do inp <- getLine
          putStrLn $ show (eval inp)