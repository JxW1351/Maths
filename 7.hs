isPrime :: Int -> Bool
isPrime 1 = False
isPrime 0 = False
isPrime x | x<0 = isPrime (-x)
          | otherwise = (primeFact x 1) == [(x,1)]

primeFact :: Int -> Int -> [(Int,Int)] -- returns [(x,y)] where x is prime and y is the greatest power of x that>
primeFact 1 _ = []
primeFact n x | x == 1 = primeFact n 2
              | x^2 > n = [(n,1)]
              | n`mod`x == 0 = ((x,y):primeFact (n`div`(x^y)) (x+1))
              | otherwise = primeFact n (x+1)
                     where y = ord x n

ord :: Int -> Int -> Int
ord 1 _ = 0
ord a n | n`mod`a == 0 = (ord a (n`div`a)) + 1
        | otherwise = 0

end :: [Int] -> Int
end [] = -1
end (x:[]) = x
end (x:xs) = end xs

calc :: Int
calc = end (take 10001 (filter isPrime [1..]))
