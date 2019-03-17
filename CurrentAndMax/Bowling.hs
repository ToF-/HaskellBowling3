module Bowling
where

score :: [Int] -> (Int,Int)
score []  = (0,300)
score [1] = (1,290)
