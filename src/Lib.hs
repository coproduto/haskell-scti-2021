module Lib
    ( hello
    , fizzBuzz
    ) where

hello :: IO ()
hello = putStrLn "Hello, world!"

fizzBuzz :: IO ()
fizzBuzz = mapM_ (putStrLn . fizzBuzzTransform) [1..100]

fizzBuzzTransform :: Int -> String
fizzBuzzTransform n
  | n `mod` 5 == 0 && n `mod` 3 == 0 = "FizzBuzz"
  | n `mod` 5 == 0                   = "Fizz"
  | n `mod` 3 == 0                   = "Buzz"
  | otherwise                        = show n
