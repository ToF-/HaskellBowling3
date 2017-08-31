module Frames.Bowling
where
import Data.Maybe

type Throw = Int
type Score = Int
data Frame = Frame Int Result 
    deriving (Eq, Show)
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
frames = tail . scanl nextFrame (Frame (-1) Normal)
    where
    nextFrame (Frame 10 _       ) r   = Frame 10 r
    nextFrame (Frame n  (Half _)) r   = Frame n  r
    nextFrame (Frame n   _      ) r   = Frame (succ n) r 

marks :: [Frame] -> [Index]
marks fs = concat $ zipWith mark fs [0..]
    where
    mark :: Frame -> Index -> [Index]
    mark (Frame n     _)  _ | n == 10 = []
    mark (Frame _ Spare)  i           = [i, i+1]
    mark (Frame _ Strike) i           = [i, i+1, i+2]
    mark _                i           = [i]
    
result :: Result -> Throw -> Result
result (Half n) t | n + t == 10 = Spare
                  | otherwise   = Normal
result _        10              = Strike
result _        t               = Half t



