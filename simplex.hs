import Control.Monad.State

phase2 :: [[Float]] -> [Float]
phase2 (x:xs) | m <= 0 = finalValues (x:xs)
              | otherwise = phase2 (pivot m n (x:xs))
                          where (m,_) = max' x
                                (n,_) = leastPositive (map (!!m) (x:xs))

leastPositive :: [Float] -> (Int,Float)
leastPositive [] = (0,0)
leastPositive (x:xs) | x<0 = (a+1,y)
                     | y<x = (a+1,y)
                     | otherwise = (0,x)
                         where (a,y) = leastPositive xs

max' :: [Float] -> (Int,Float)
max' [] = (0,-9999)
max' (x:xs) | x<y = (a+1,y)
            | otherwise = (0,x)
                 where (a,y) = leastPositive xs


finalValues :: [[a]] -> [a]
finalValues [] = []
finalValues ((x:[]):ys) = (x:finalValues ys)
finalValues ((x:xs):ys) = finalValues ((xs):ys)

pivot :: Int -> Int -> [[Float]] -> [[Float]]
pivot m n xs = tableau 
                where ((),tableau) = runState (pivotMap m n 0 (length xs)) xs

pivotMap :: Int -> Int -> Int -> Int -> State [[Float]] ()
pivotMap m n x y | y == x = pure()
                 | otherwise = do 
                                modify (divRow m n)
                                modify (subRow m n)

divRow :: Int -> Int -> [[Float]] -> [[Float]]
divRow m 0 (x:xs) = (map (\y -> y/z) x:xs)
                        where z = x!!m
divRow m n (x:xs) = divRow m (n-1) xs

subRow :: Int -> Int -> [[Float]] -> [[Float]]
subRow m n xs = map (\x -> subRow' n x (xs!!m)) xs

subRow' :: Int -> [Float] -> [Float] -> [Float]
subRow' n (x:xs) (y:ys) = ((x-a*y):subRow' n xs ys)
                             where a = (y:ys)!!n
