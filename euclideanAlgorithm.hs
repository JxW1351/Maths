calc :: (Int,Int) -> (Int,Int) -> (Int,Int)
calc a (0,0) = a
calc a b = (calc b (q a b))

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
