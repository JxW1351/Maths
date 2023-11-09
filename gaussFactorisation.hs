help = "Takes an input of (a,b) and returns a list of pairs (c,d) such that the list is a factorisation of a+bi into Irreducibles c+di in the Gaussian Integers Z[i]"

norm :: (Int,Int) -> Int
norm (a,b) = a^2 + b^2 

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 0 = False
isPrime x | x<0 = isPrime (-x)
          | otherwise = length (filter (\c -> x`mod`c==0) [1..(floor(sqrt(fromIntegral x)))])

calc :: (Int,Int) -> [(Int,Int)]
calc (a,0) | isPrime a = primeFact (a,0)
           | otherwise = do
          let n = head(filter (\c -> n`mod`c==0) [2..])
calc (a,b) | isPrime (norm (a,b)) = primeFact (a,b)

primeFact :: (Int,Int) -> [(Int,Int)]
primeFact (a,0) | a==2 = [(1,1),(1,-1)]
                | (a^2 `mod` 4 == 1) = fromNorm a
                | otherwise = [(a,0)]
primeFact (a,b) = undefined

fromNorm :: Int -> [(Int,Int)]
fromNorm a = undefined
