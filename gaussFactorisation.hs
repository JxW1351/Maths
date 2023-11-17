help = "Takes an input of (a,b) and returns a list of pairs (c,d) such that the list is a factorisation of a+bi into Irreducibles c+di in the Gaussian Integers Z[i] (CURRENTLY UNFINISHED)"

norm :: (Int,Int) -> Int
norm (a,b) = a^2 + b^2

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 0 = False
isPrime x | x<0 = isPrime (-x)
          | otherwise = length (filter (\c -> x`mod`c==0) [1..(floor(sqrt(fromIntegral x)))]) == 1

primeFact :: Int -> [Int]
primeFact 1 = []
primeFact n = (x: primeFact (n`div`x))
         where x = head(filter (\c -> n`mod`c==0) [2..])

irred :: (Int,Int) -> Bool
irred (a,b) | isPrime n = True
            | a==0 = isSqPrime b
            | b==0 = isSqPrime a
            | otherwise = isSqPrime n && (n`mod`4==1)
              where n = norm(a,b)

isSqPrime :: Int -> Bool
isSqPrime n = x^2 == n && isPrime x
         where x = floor(sqrt(fromIntegral n))

fromNorm :: Int -> (Int,Int) -> (Int,Int)
fromNorm n m = head(filter (\(x,y) -> ((isFact (x,y) m)||(isFact (x,-y) m)) && (x^2 + y^2 == n)) [(x,y) | x<-[1..],y<-[0..x]])

fromNormWithConjugate :: Int -> (Int,Int) -> (Int,Int)
fromNormWithConjugate n m | isFact (x,y) m = (x,y)
                          | otherwise = (x,-y)
                            where (x,y) = fromNorm n m

isFact :: (Int,Int) -> (Int,Int) -> Bool
isFact (a,b) (c,d) = ((a*c)+(b*d))`mod`n==0 && ((a*d)-(b*c))`mod`n==0
                     where n = norm(a,b)

calc :: (Int,Int) -> [(Int,Int)]
calc (a,b) | mult x == (a,b) = x
           | mult x == (-a,-b) = ((-1,0):x)
           | mult x == (b,-a) = ((0,1):x)
           | mult x == (-b,a) = ((0,-1):x)
           | otherwise = x
             where x = map (\x -> fromNormWithConjugate x (a,b)) (primeFact (norm (a,b)))

mult :: [(Int,Int)] -> (Int,Int)
mult (a:[]) = a
mult ((a,b):((x,y):ys)) = mult ((a*x-b*y,a*y+b*x):ys)
