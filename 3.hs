primeFact :: Int -> Int -> [(Int,Int)] -- returns [(x,y)] where x is prime and y is>
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

max' :: [(Int,Int)] -> Int
max' [] = 0
max' ((x,_):xs) = max x (max' xs)

calc :: Int
calc = max' (primeFact 600851475143 1)
