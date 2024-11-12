step :: Int -> Int
step n | even n = n `div` 2
step n | otherwise = 1 + 3 * n

isEven :: Int -> Bool
isEven n = n `div` 2 == n

steps :: Int -> Int
steps 1 = 1
steps n = (steps (step n)) + 1

steps' :: Int -> (Int,Int)
steps' n = (n,steps n)

max' :: (Int,Int) -> (Int,Int) -> (Int,Int)
max' (x,m) (y,n) | m < n = (y,n)
                 | otherwise = (x,m)

main :: IO()
main = do
    let result = foldl max' (1, 0) (map steps' [1..1000000])
    print result
