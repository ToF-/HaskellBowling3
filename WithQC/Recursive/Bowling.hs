module Recursive.Bowling (score) where

type Throw = Int
type Score = Int

score :: [Throw] -> Score
score = score' 0
    where
    score' :: Int -> [Throw] -> Score
    score' f [] = 0
    score' 10 _ = 0
    score' f (t:u:v:ts) | t == 10   = t+u+v + score' (succ f) (u:v:ts)
    score' f (t:u:v:ts) | t+u == 10 = t+u+v + score' (succ f) (v:ts)
    score' f (t:u:ts)               = t+u   + score' (succ f) (ts)
    score' f (t:ts)                 = t + score' f ts
    


