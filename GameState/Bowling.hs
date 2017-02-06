module Bowling
where

data GameState = GameState { _Score :: Score,
                             _Bonus :: Bonus,
                             _Last  :: Maybe Throw,
                             _Frame :: Frame}

    deriving (Eq,Show)

type Throw = Int
type Score = Int
type Bonus = (Int,Int)
type Frame = Int

score :: [Throw] -> Score
score = _Score . foldl add initial 

add :: GameState -> Throw -> GameState
add (GameState sc b l f) t = GameState { _Score = newScore sc b t f, 
                                         _Bonus = newBonus b l t f,
                                         _Last  = newLast l t,
                                         _Frame = newFrame f l t}
    where 
    newScore :: Score -> Bonus -> Throw -> Frame -> Score
    newScore sc (n,_) t 10 = sc + t 
    newScore sc (n,_) t f  = sc + t + t * n

    newBonus :: Bonus -> Maybe Throw -> Throw -> Frame -> Bonus
    newBonus (_,b) _ _ 10                        = (b,0)
    newBonus (_,b) Nothing 10 _                  = (b+1,1)
    newBonus (_,b) l t _ | fmap (+t) l == Just 10 = (1,0)  
                         | otherwise              = (b,0)

    newLast :: Maybe Throw -> Throw -> Maybe Throw
    newLast Nothing 10 = Nothing
    newLast (Just _) _ = Nothing
    newLast Nothing  t = Just t

    newFrame :: Frame -> Maybe Throw -> Throw -> Frame
    newFrame f (Just _) _  = succ f
    newFrame f Nothing  10 = succ f
    newFrame f Nothing  _  = f
    
initial :: GameState 
initial = GameState { _Score = 0, _Bonus = (0,0), _Last = Nothing, _Frame = 0 }


