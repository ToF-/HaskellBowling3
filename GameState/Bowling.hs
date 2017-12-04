module GameState.Bowling
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
add (GameState score (bonus1,bonus2) last frame) throw = GameState score' bonus' last' frame'
    where 
    score' = score + th + (throw * bonus1)
        where th | frame < 10    = throw
                 | otherwise = 0

    bonus'  | frame == 10                    = (bonus2,0)
            | last == Nothing && throw == 10 = (bonus2+1,1)
            | fmap (+throw) last == Just 10  = (1,0)
            | otherwise                      = (bonus2,0)

    last' | last == Nothing && throw == 10   = Nothing
          | last == Nothing                  = Just throw
          | otherwise                        = Nothing

    frame' | frame == 10        = 10
           | last /= Nothing || throw == 10   = succ frame
           | otherwise          = frame
    
initial :: GameState 
initial = GameState 0 (0,0) Nothing 0 


