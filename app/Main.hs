module Main where

import Lib (hello, fizzBuzz, fizzBuzzInteractive)
import NewtonRaphson (findRoot)
import Fibonacci (fib)

import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Insira um número positivo:"
  numInput <- getLine
  putStrLn "Insira qual raiz você quer (inteiro, n > 0):"
  nthRootInput <- getLine
  case (readMaybe numInput :: Maybe Double, readMaybe nthRootInput :: Maybe Int) of
    (Just n, Just k)
      | n > 0 && k > 0 -> putStrLn ("Resultado: " ++ (show (findRoot k n)))
      | otherwise      -> putStrLn "Entrada inválida (números não-positivos)"

    _ -> putStrLn "Entrada inválida."
