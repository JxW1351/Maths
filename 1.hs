calc :: Int
calc = sum(map (\x -> x*3) [1..333]) + sum(map (\x -> x*5) [1..199]) - sum(map (\x -> x*15) [1..1000`div`15])
