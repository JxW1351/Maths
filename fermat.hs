calc :: Int -> Bool
calc 0 = True
calc x | x > 0 = fermat x
       | otherwise = False

fermat :: Int -> Bool
fermat 1 = True
fermat x | x`mod`2==0 = fermat (x`div`2)
         | otherwise = fermat2 x (head (filter (\y -> x `mod` y == 0) [2..]))

fermat2 :: Int -> Int -> Bool
fermat2 x n | n`mod`4 == 1 = fermat (x`div`n)
            | x`mod`(n^2)==0 = fermat (x`div`(n^2))
            | otherwise = False
