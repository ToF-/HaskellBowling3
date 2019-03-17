module Bowling
where

data Frame = Regular Int Int
           | Spare Int 
           | Strike
           | Final Frame (Maybe Int) (Maybe Int)
score :: [Int] -> (Int,Int)
score []  = (0,300)
score [1] = (1,290)
