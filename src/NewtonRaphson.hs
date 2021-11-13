module NewtonRaphson where

findSqrt :: Double -> Double
findSqrt n = findSqrt' n 100 (n / 2)

findSqrt' _ 0     guess = guess
findSqrt' n iters guess = findSqrt' n (iters - 1) (updateGuessSqrt n guess)

updateGuessSqrt n guess = guess - (guess * guess - n) / (2 * guess)

findCbrt :: Double -> Double
findCbrt n = findCbrt' n 100 (n / 3)

findCbrt' _ 0     guess = guess
findCbrt' n iters guess = findCbrt' n (iters - 1) (updateGuessCbrt n guess)

updateGuessCbrt n guess = guess - (guess * guess * guess - n) / (3 * guess * guess)

findRoot :: Int -> Double -> Double
findRoot n x
  | n <  0    = error "Sem números complexos por enquanto, dsclp"
  | n == 0    = error "Raiz zerésima é indefinida!"
  | otherwise = findRootIter n x 100 (x / (fromIntegral n))

findRootIter :: Int -> Double -> Int -> Double -> Double
findRootIter _   _      0     guess = guess
findRootIter nth target iters guess =
  findRootIter nth target (iters - 1) (updateGuess nth target guess)

updateGuess :: Int -> Double -> Double -> Double
updateGuess nth target guess =
  guess - (((pow guess nth) - target) / ((fromIntegral nth) * (pow guess (nth - 1))))

pow :: Double -> Int -> Double
pow _ 0 = 1
pow m n
  | n < 0     = 1 / (pow m (negate n))
  | otherwise = m * (pow m (n - 1))
