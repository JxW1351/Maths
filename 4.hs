isPalindrome :: Int -> Bool
isPalindrome n = show n == reverse (show n)

calc :: Int
calc = max' (filter isPalindrome (expand(map (\x -> (map (\y -> x*y) [100..999])) [100..999])))

expand :: [[Int]] -> [Int]
expand [] = []
expand (x:xs) = x ++ expand xs

max' :: [Int] -> Int
max' [] = 0
max' (x:xs) = max x (max' xs)
