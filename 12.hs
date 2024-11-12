factors :: Int -> [Int]
factors n = filter (\x -> n`mod`x==0) (takeWhile (\x -> x^2<n) [1..])

factors' :: Int -> [Int]
factors' n = expand (map (\x -> (fact n x)) (factors n))

expand :: [[Int]] -> [Int]
expand [] = []
expand (x:xs) = x ++ expand xs

fact :: Int -> Int -> [Int]
fact n x | n == x^2 = [x]
         | otherwise = [x,n`div`x]

triangle :: Int -> Int
triangle n = sum([1..n])

calc :: Int
calc = head(filter (\n -> length(factors' n)>498) (map triangle [1..]))

main :: IO()
main = do
    let result = calc
    print result
