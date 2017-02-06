module Bowling
where

data GameState = GameState { _Score :: Score,
                             _Bonus :: Bonus,
                             _Last  :: Maybe Throw}
    deriving (Eq,Show)

type Throw = Int
type Score = Int
type Bonus = (Int,Int)

score :: [Throw] -> Score
score = _Score . foldl add initial 

add :: GameState -> Throw -> GameState
add (GameState sc b l) t = GameState { _Score = newScore sc b t, 
                                       _Bonus = newBonus b l t,
                                       _Last  = newLast l t}
    where 
    newScore :: Score -> Bonus -> Throw -> Score
    newScore sc (n,_) t = sc + t + t * n

    newBonus :: Bonus -> Maybe Throw -> Throw -> Bonus
    newBonus (_,b) Nothing 10                   = (b+1,1)
    newBonus (_,b) l t | fmap (+t) l == Just 10 = (1,0)  
                       | otherwise              = (b,0)

    newLast :: Maybe Throw -> Throw -> Maybe Throw
    newLast Nothing 10 = Nothing
    newLast (Just _) _ = Nothing
    newLast Nothing  t = Just t
    
initial :: GameState 
initial = GameState { _Score = 0, _Bonus = (0,0), _Last = Nothing }


