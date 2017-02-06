module Bowling
where

type Throw = Int

score :: [Throw] -> Int
score = sum
