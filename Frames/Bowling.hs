module Bowling
where
import Data.Maybe

type Throw = Int
type Score = Int
type Frame = (Int,Result)
type Index = Int

data Result = Normal | Half Int | Spare | Strike
    deriving (Eq,Show)

score :: [Throw] -> Score
score ts = collect ts $ marks $ frames $ results ts
    where 
    collect :: [Throw] -> [Index] -> Score
    collect ts ps = sum $ map throwAt ps
        where
        game :: [(Int,Throw)]
        game = zip [0..] ts

        throwAt :: Index -> Throw
        throwAt = fromJust . flip lookup game

results :: [Throw] -> [Result]
results = tail . scanl result Normal 

frames :: [Result] -> [Frame]
frames = tail . scanl frame (-1,Normal)
    where
    frame (10,_) r       = (10, r)
    frame (f,Half _) r   = (f, r)
    frame (f,_)      r   = (succ f, r)

marks :: [Frame] -> [Index]
marks fs = concat $ zipWith mark fs [0..]
    where
    mark :: Frame -> Index -> [Index]
    mark (f,_) _ | f == 10 = []
    mark (_,Spare)  i   = [i, i+1]
    mark (_,Strike) i   = [i, i+1, i+2]
    mark (_,_)      i   = [i]
    
result :: Result -> Throw -> Result
result (Half n) t | n + t == 10 = Spare
                  | otherwise   = Normal
result _        10              = Strike
result _        t               = Half t



