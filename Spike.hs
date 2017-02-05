import Data.List
import Data.Maybe

data Score = Strike | Spare | Normal | Half Int 
    deriving (Eq, Show,Ord)

type Throw = Int
type Frame = (Int,Score)

result :: Score -> Throw -> Score
result (Half x) y | x+y == 10 = Spare
                 | otherwise = Normal
result _  10 = Strike
result _   x = Half x

scoring :: [Throw] -> [Score]
scoring ns = scoring' Normal ns
    where 
    scoring' _ [] = []
    scoring' s (t:ts) = sc' : scoring' sc' ts
        where
        sc' = result s t

framing :: [Score] -> [Frame]
framing ss = framing' 0 ss
    where 
    framing' _ [] = []
    framing' f (Half n:ss) = (f,(Half n)):framing' f ss    
    framing' f (sc:ss)     = (f,sc):framing' (f+1) ss

bonusing :: [Frame] -> [Int]
bonusing fs = sort $ concat $ zipWith bonusing' [0..] fs
    where
    bonusing' t (f,_) | f >= 10 = [] 
    bonusing' t (_,Strike) = [t,t+1,t+2]
    bonusing' t (_,Spare)  = [t,t+1]
    bonusing' t (_,_)      = [t]
    

score :: [Throw] -> Int
score ts =
    let throws   = zip [0..] ts
        bonuses  = bonusing $ framing $ scoring ts
        points = map (fromJust . flip lookup throws) bonuses 
    in sum points

ts = [6,4,10,0,10,3,5]
pf = replicate 12 10
main = do
    print ts
    print $ scoring ts
    print $ framing $ scoring ts
    print $ bonusing $ framing $ scoring ts
    print $ score ts
    print pf
    print $ scoring pf
    print $ framing $ scoring pf
    print $ bonusing $ framing $ scoring pf
    print $ score pf
    

