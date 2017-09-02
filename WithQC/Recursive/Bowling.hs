module Recursive.Bowling (score) where

type Throw = Int
type Score = Int

score :: [Throw] -> Score
score ts = sum ts


