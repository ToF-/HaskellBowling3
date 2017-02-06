module Bowling
where

data GameState = GameState { _score :: Score,
                             _bonus :: Bonus,
                             _last  :: Last,
                             _frame :: Frame}

    deriving (Eq,Show)

type Throw = Int
type Score = Int
type Bonus = (Int,Int)
type Last  = Maybe Throw
type Frame = Int

score :: [Throw] -> Score
score = _score . foldl add initial 

add :: GameState -> Throw -> GameState
add g@(GameState sc b l f) t = GameState { _score = newScore g t,
                                           _bonus = newBonus g t,
                                           _last  = newLast l t,
                                           _frame = newFrame f l t}
    where 
    newScore :: GameState -> Throw -> Score
    newScore (GameState sc (n,_) _ 10) t = sc + 0 + (t * n) 
    newScore (GameState sc (n,_) _ _) t = sc + t + (t * n)

    newBonus :: GameState -> Throw -> Bonus
    newBonus (GameState _ (_,b) _      10) _  = (b,0)
    newBonus (GameState _ (_,b) Nothing _) 10 = (b+1,1)
    newBonus (GameState _ (_,b) l       _) t | fmap (+t) l == Just 10 = (1,0)
                                             | otherwise              = (b,0)

    newLast :: Last -> Throw -> Last
    newLast Nothing 10 = Nothing
    newLast (Just _) _ = Nothing
    newLast Nothing  t = Just t

    newFrame :: Frame -> Last -> Throw -> Frame
    newFrame 10 _       _  = 10
    newFrame f (Just _) _  = succ f
    newFrame f Nothing  10 = succ f
    newFrame f Nothing  _  = f
    
initial :: GameState 
initial = GameState { _score = 0, _bonus = (0,0), _last = Nothing, _frame = 0 }


