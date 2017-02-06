module Bowling
where
import Data.Maybe

type Throw = Int
type Score = Int

score :: [Throw] -> Score
score ts = points ts [0..(length ts - 1)]
    where 
    points :: [Throw] -> [Int] -> Score
    points ts ps = sum $ map (fromJust . flip lookup game) ps
        where
        game :: [(Int,Throw)]
        game = zip [0..] ts
    




