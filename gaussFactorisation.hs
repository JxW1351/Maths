help = "Takes an input of (a,b) and returns a list of pairs (c,d) such that the list is a factorisation of a+bi into Irreducibles c+di (with one unit to ensure the product is equal to the input) in the Gaussian Integers Z[i]"

pretty :: (Int,Int) -> (Int,Int) -- formats values with positive real part of greater modulus than the imaginary part
pretty (x,y) | x>0 && x>=b = (x,y)
             | x>0 && y>x = (y,-x)
             | x>0 = (-y,x)
             | x==0 && y>0 = (y,0)
             | x==0 = (-y,0)
             | y>0 && y>=a = (y,-x)
             | y>0 = (-x,-y)
             | b>=a = (-y,x)
             | otherwise = (-x,-y)
                where a | x<0 = -x
                        | otherwise = x
                      b | y<0 = -y
                        | otherwise = y


norm :: (Int,Int) -> Int
norm (a,b) = a^2 + b^2

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 0 = False
isPrime x | x<0 = isPrime (-x)
          | otherwise = (primeFact x 1) == [(x,1)]

primeFact :: Int -> Int -> [(Int,Int)] -- returns [(x,y)] where x is prime and y is the greatest power of x that is a factor of n
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


fromNorm :: Int -> (Int,Int) -- returns a gaussian integer with norm n
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
calc2 (a,b) = map pretty (concat (map (\x -> (fromNormWithConjugate x (a,b))) (primeFact (norm (a,b)) 1)))

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
