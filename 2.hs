calc :: Int
calc = sum (filter even (fib []))

fib :: [Int] -> [Int]
fib [] = fib [1]
fib [x] = fib (x+1:[x])
fib (x:(y:ys)) | (x+y) <= 4000000 = fib (x+y:(x:(y:ys)))
               | otherwise = (x:(y:ys))
