module Lib
    ( hello
    , fizzBuzz
    , fizzBuzzInteractive
    ) where

import Text.Read (readMaybe)

hello :: IO ()
hello = putStrLn "Hello, world!"

fizzBuzz :: IO ()
fizzBuzz = fizzBuzzN 100

fizzBuzzN :: Int -> IO ()
fizzBuzzN n = mapM_ (putStrLn . fizzBuzzTransform) [1..n]

fizzBuzzInteractive :: IO ()
fizzBuzzInteractive = do
  putStrLn "Insira um número positivo:"
  inputLine <- getLine
  case (readMaybe inputLine :: Maybe Int) of
    Just n
      | n > 0     -> fizzBuzzN n
      | otherwise -> do
          putStrLn "O número inserido não é positivo."
          fizzBuzzInteractive

    Nothing -> do
      putStrLn "O valor inserido não é um número válido."
      fizzBuzzInteractive
  

fizzBuzzTransform :: Int -> String
fizzBuzzTransform n
  | n `mod` 5 == 0 && n `mod` 3 == 0 = "FizzBuzz"
  | n `mod` 5 == 0                   = "Fizz"
  | n `mod` 3 == 0                   = "Buzz"
  | otherwise                        = show n
