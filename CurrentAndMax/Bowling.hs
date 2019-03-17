module Bowling (score)
where

score :: [Int] -> (Int,Int)
score ts = (current 0 ts, scoreMax ts)

current :: Int -> [Int] -> Int
current 10 _ = 0
current n [] = 0
current n (x:y:z:ts) | x == 10 = 10 + y + z + current (succ n) (y:z:ts)
current n (x:y:z:ts) | x+y == 10 = 10 + z + current (succ n) (z:ts)
                     | otherwise = x+y + current (succ n) (z:ts)
current n [x,y] = x + y
current n [x]   = x

scoreMax :: [Int] -> Int
scoreMax ts | halfFrame ts = current 0 (ts ++ ([10 - (last ts)] ++ replicate 11 10)) 
            | otherwise    = current 0 (ts ++ replicate 12 10)

halfFrame :: [Int] -> Bool
halfFrame [] = False
halfFrame (10:ts) = halfFrame ts
halfFrame (x:y:ts) = halfFrame ts
halfFrame [x] = True

