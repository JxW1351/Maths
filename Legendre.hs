legendre :: Int -> Int -> Int
legendre 0 _ = 0
legendre 1 _ = 1
legendre 2 p | p `mod` 8 == 1 = 1
             | p `mod` 8 == 7 = 1
             | otherwise = -1
legendre a p | isSquare a = 1
             | (a<0) && p`mod`4 == 1 = legendre (-a) p
             | (a<0) = -(legendre (-a) p)
             | (isPrime a && a<p && a`mod`4==3 && p`mod`4==3) = -legendre p a
             | (isPrime a && a<p) = legendre p a
             | a>(p`div`2) = legendre (leastRes a p) p
             | otherwise = legendreFactors a p

legendreFactors :: Int -> Int -> Int
legendreFactors a p = (legendre x p) * (legendre (a`div`x) p)
  where x = head(filter (factor a) [2..floor(sqrt(fromIntegral(a)))])
                   
                       

minFac :: Int -> Int
minFac x = head (filter (factor x) [2..(floor(sqrt(fromIntegral(x))))])

isSquare :: Int -> Bool
isSquare x = x == (floor(sqrt(fromIntegral(x))))^2

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = False
isPrime x = length (filter (factor x) [1..(floor(sqrt(fromIntegral(modulus x))))]) == 1

modulus :: Int -> Int
modulus x | x < 0 = (-x)
          | otherwise =x

factor :: Int -> Int -> Bool
factor n a = n`mod`a == 0

res :: Int -> Int -> Int
res a p = a - (p * (a `div` p))

leastRes :: Int -> Int -> Int
leastRes a p | (res a p) > (p`div`2) = (res a p) - p
             | otherwise = (res a p) 

calc :: Int -> Int -> Int
calc a p | isPrime p = legendre a p
         | otherwise = -(minBound)
