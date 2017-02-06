module Bowling
where

type Throw = Int

score :: [Throw] -> Int
score [] = 0
score (x:y:z:ts) | x == 10   = x+y+z + score (y:z:ts)
score (x:y:z:ts) | x+y == 10 = x+y+z + score (z:ts)
                 | otherwise = x+y   + score (z:ts)
score (x:ts)                 = x     + score ts
