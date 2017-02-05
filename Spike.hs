import Data.List
import Data.Maybe

data Result = Strike | Spare | Normal | Half Int 
    deriving (Eq, Show,Ord)

type Throw = Int
type Frame = (Int,Result)

result :: Result -> Throw -> Result
result (Half x) y | x+y == 10 = Spare
                  | otherwise = Normal
result _  10                  = Strike
result _   x                  = Half x

results :: [Throw] -> [Result]
results = results' Normal
    where 
    results' _ [] = []
    results' s (t:ts) = let r = result s t in r : results' r ts

frames :: [Result] -> [Frame]
frames rs = frames' 0 rs
    where 
    frames' _ [] = []
    frames' f (Half n:rs) = (f,(Half n)):frames' f rs    
    frames' f (r:rs)      = (f,r)       :frames' (succ f) rs

bonuses :: [Frame] -> [Int]
bonuses fs = concat $ zipWith bonuses' [0..] fs
    where
    bonuses' t (f,_) | f >= 10 = [] 
    bonuses' t (_,Strike) = [t,t+1,t+2]
    bonuses' t (_,Spare)  = [t,t+1]
    bonuses' t (_,_)      = [t]
    

score :: [Throw] -> Int
score ts = (sum . map (fromJust . flip lookup (zip [0..] ts)) . (bonuses . frames . results)) ts 

ts = [6,4,10,0,10,3,5]
pf = replicate 12 10
main = do
    print ts
    print $ results ts
    print $ frames $ results ts
    print $ bonuses $ frames $ results ts
    print $ score ts
    print pf
    print $ results pf
    print $ frames $ results pf
    print $ bonuses $ frames $ results pf
    print $ score pf
    

