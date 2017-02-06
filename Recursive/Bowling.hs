module Bowling
where

type Throw = Int

score :: [Throw] -> Int
score = score' 0
    where
    score' :: Int -> [Throw] -> Int
    score' 10 _ = 0
    score' _ [] = 0
    score' f (x:y:z:ts) | x == 10   = x+y+z + score' (succ f) (y:z:ts)
    score' f (x:y:z:ts) | x+y == 10 = x+y+z + score' (succ f) (z:ts)
                        | otherwise = x+y   + score' (succ f) (z:ts)
    -- this pattern matches partial games (half frame)
    score' f (x:ts)                 = x     + score' f ts

