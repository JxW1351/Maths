calc :: Int
calc = prod (head (filter (\(x,y,z) -> x+y+z==1000) (map triples(expand (map mn [1..30])))))

prod :: (Int,Int,Int) -> Int
prod (x,y,z) = x*y*z

expand :: [[(Int,Int)]] -> [(Int,Int)]
expand [] = []
expand (x:xs) = x ++ expand xs

triples :: (Int,Int) -> (Int,Int,Int)
triples (m,n) = (m^2-n^2,2*m*n,m^2+n^2)

mn :: Int -> [(Int,Int)]
mn n = map (\x -> (n,x)) [1..n-1]
