module Primes where

nthPrime :: Int -> Integer
nthPrime = (primes !!)

primes :: [Integer]
primes = unfold sieveStep [2..]

unfold :: ([Integer] -> (Integer, [Integer])) -> [Integer] -> [Integer]
unfold step input =
  let (x, nextInput) = step input
  in case nextInput of
    []   -> [x]
    next -> x : (unfold step next)

sieveStep :: [Integer] -> (Integer, [Integer])
sieveStep (x:xs) = (x, filter (not . (x `divides`)) xs)

divides :: Integer -> Integer -> Bool
divides x n = n `mod` x == 0
