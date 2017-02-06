module Bowling
where
import Data.Maybe

type Throw = Int
type Score = Int
type Frame = (Int,Result)

data Result = Normal | Half Int | Spare | Strike
    deriving (Eq,Show)

score :: [Throw] -> Score
score ts = points ts $ marks $ frames $ results ts
    where 
    points :: [Throw] -> [Int] -> Score
    points ts ps = sum $ map (fromJust . flip lookup game) ps
        where
        game :: [(Int,Throw)]
        game = zip [0..] ts

results :: [Throw] -> [Result]
results = tail . scanl result Normal 

frames :: [Result] -> [Frame]
frames = tail . scanl frame (-1,Normal)
    where
    frame (10,_) r       = (10, r)
    frame (f,Half _) r   = (f, r)
    frame (f,_)      r   = (succ f, r)

marks :: [Frame] -> [Int]
marks fs = concat $ zipWith mark fs [0..]
    where
    mark :: Frame -> Int -> [Int]
    mark (f,_) _ | f == 10 = []
    mark (_,Normal) t   = [t]
    mark (_,Half _) t   = [t]
    mark (_,Spare)  t   = [t, t+1]
    mark (_,Strike) t   = [t, t+1, t+2]
    
result :: Result -> Throw -> Result
result (Half n) t | n + t == 10 = Spare
                  | otherwise   = Normal
result _        10              = Strike
result _        t               = Half t



