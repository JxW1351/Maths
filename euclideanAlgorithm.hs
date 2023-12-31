calc :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
calc a b | x==0 = [(y,0),(0,y),(0,-y),(-y,0)]
         | (x < 0)&&(y<0) = [(-x,-y),(-y,x),(x,y),(y,-x)]
         | x<0 = [(y,-x),(-x,-y),(x,y),(-y,x)]
         | y<0 = [(-y,x),(x,y),(-x,-y),(y,-x)]
         | otherwise = [(x,y),(y,-x),(-y,x),(-x,-y)]
        where (x,y) = euc a b

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

help = "calc (a,b) (c,d) returns a list of all pairs (e,f) such that e+fi is a Highest Common Factor of a+bi and c+di in the Gaussian Integers Z[i], and euc (a,b) (c,d) returns just one highest common factor"
