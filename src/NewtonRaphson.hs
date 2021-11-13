module NewtonRaphson where

findSqrt :: Double -> Double
findSqrt = findRoot 2

findCbrt :: Double -> Double
findCbrt = findRoot 3

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
