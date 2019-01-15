module Bowling where

type Throw = Integer

score :: [Throw] -> Integer
score = score' 0
score' :: Integer -> [Throw] -> Integer
score' 10 _ = 0
score' f [] = 0
score' f (10:y:z:xs) = 10 + y + z + score' (f+1) (y:z:xs)
score' f (x:y:z:xs) | x + y == 10 = 10 + z + score' (f+1) (z:xs)
                    | otherwise = x + y + score' (f+1) (z:xs)
score' f (10:y:xs) = 10 + y + score' (f+1) (y:xs)
score' f (x:y:xs) = x + y + score' (f+1) xs
score' f (x:xs) = x + score' f xs
