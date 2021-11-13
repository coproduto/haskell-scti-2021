module NewtonRaphson where

findSqrt :: Double -> Double
findSqrt n = findSqrt' n 100 (n / 2)

findSqrt' _ 0     guess = guess
findSqrt' n iters guess = findSqrt' n (iters - 1) (updateGuess n guess)
  
updateGuess n guess = guess - (guess * guess - n) / (2 * guess)
