help = "Takes an input of (a,b) and returns a list of pairs (c,d) such that the list is a factorisation of a+bi into Irreducibles c+di in the Gaussian Integers Z[i] (CURRENTLY UNFINISHED)"

pretty :: (Int,Int) -> (Int,Int)
pretty (x,y) | x<0 && y>0 = (y,-x)
             | x<0 = (-x,-y)
             | x==0 && y<0 = (-y,0)
             | x==0 = (y,0)
             | y<0 = (-y,x)
             | otherwise = (x,y)

norm :: (Int,Int) -> Int
norm (a,b) = a^2 + b^2

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 0 = False
isPrime x | x<0 = isPrime (-x)
          | otherwise = length (filter (\c -> x`mod`c==0) [1..(floor(sqrt(fromIntegral x)))]) == 1

primeFact :: Int -> [(Int,Int)]
primeFact 1 = []
primeFact n | isPrime n = [(n,1)]
            | x`mod`4==3 = ((x,2*y): primeFact (n`div`(x^(2*y))))
            | otherwise = ((x,y):primeFact (n`div`(x^y)))
              where x = head(filter (\c -> n`mod`c==0) [2..])
                    y = ord x n

ord :: Int -> Int -> Int
ord 1 _ = 0
ord a n | n`mod`a == 0 = (ord a (n`div`a)) + 1
        | otherwise = 0


fromNorm :: Int -> (Int,Int)
fromNorm n = euc (head (filter (\m -> (m^2) `mod` n == (n-1)) [1..(n`div`2)]),1) (n,0)

fromNormWithConjugate :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
fromNormWithConjugate (_,0) _ = []
fromNormWithConjugate (2,n) m | isFact (1,1) m = ((1,1):fromNormWithConjugate (2,n-1) (quotient m (1,1)))
                              | otherwise = ((1,-1):fromNormWithConjugate (2,n-1) (quotient m (1,-1)))
fromNormWithConjugate (n,j) m | n`mod`4==3 = replicate (j`div`2) (n,0)
                              | isFact (x,y) m = ((x,y):fromNormWithConjugate (n,j-1) (quotient m (x,y)))
                              | otherwise = ((x,-y):fromNormWithConjugate (n,j-1) (quotient m (x,-y)))
                                where (x,y) = fromNorm n

isFact :: (Int,Int) -> (Int,Int) -> Bool
isFact (a,b) (c,d) = ((a*c)+(b*d))`mod`n==0 && ((a*d)-(b*c))`mod`n==0
                     where n = norm(a,b)

calc :: (Int,Int) -> [(Int,Int)]
calc (a,b) | mult x == (a,b) = x
           | mult x == (-a,-b) = ((-1,0):x)
           | mult x == (b,-a) = ((0,1):x)
           | mult x == (-b,a) = ((0,-1):x)
           | otherwise = ((0,0):x) --FOR DEBUG
             where x = calc2 (a,b)

calc2 :: (Int,Int) -> [(Int,Int)]
calc2 (a,b) = map pretty (concat (map (\x -> (fromNormWithConjugate x (a,b))) (primeFact (norm (a,b)))))

mult :: [(Int,Int)] -> (Int,Int)
mult (a:[]) = a
mult ((a,b):((x,y):ys)) = mult ((a*x-b*y,a*y+b*x):ys)



-- FROM euclideanAlgorithm.hs



euc :: (Int,Int) -> (Int,Int) -> (Int,Int)
euc a (0,0) = a
euc a b = (euc b (q a b))

rdiv :: Int -> Int -> Int
rdiv a b = round ((fromIntegral a) / (fromIntegral b))

quotient :: (Int,Int) -> (Int,Int) -> (Int,Int)
quotient (a,b) (c,d) = (((a*c)+(b*d)) `rdiv` (c^2 + d^2),((b*c)-(a*d)) `rdiv` (c^2+d^2))

modulo :: (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int)
modulo (a,b) (c,d) (e,f) = ((a-(c*e - d*f)),(b-(c*f + d*e)))

qm :: (Int, Int) -> (Int,Int) -> ((Int,Int),(Int,Int))
qm a b = do
  let x = (quotient a b)
  (x,modulo a b x)

q :: (Int, Int) -> (Int,Int) -> (Int,Int)
q a b = do
  let x = (quotient a b)
  modulo a b x
