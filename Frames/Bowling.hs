module Bowling
where
import Data.Maybe

type Throw = Int
type Score = Int

data Result = Normal | Half Int | Spare | Strike
    deriving (Eq,Show)

score :: [Throw] -> Score
score ts = points ts (marks (results ts))
    where 
    points :: [Throw] -> [Int] -> Score
    points ts ps = sum $ map (fromJust . flip lookup game) ps
        where
        game :: [(Int,Throw)]
        game = zip [0..] ts

results :: [Throw] -> [Result]
results = tail . scanl result Normal 

marks :: [Result] -> [Int]
marks = concat . zipWith mark [0..]
    where
    mark t Normal   = [t]
    mark t (Half _) = [t]
    mark t Spare    = [t, t+1]
    mark t Strike   = [t, t+1, t+2]
    
result :: Result -> Throw -> Result
result (Half n) t | n + t == 10 = Spare
                  | otherwise   = Normal
result _        10              = Strike
result _        t               = Half t



