module Bowling
where

data GameState = GameState { _Score :: Score,
                             _Bonus :: Bonus,
                             _Last  :: Maybe Throw}
type Throw = Int
type Score = Int
type Bonus = Bool

score :: [Throw] -> Score
score = _Score . foldl add initial 
    where
    initial :: GameState 
    initial = GameState { _Score = 0, _Bonus = False, _Last = Nothing }

    add :: GameState -> Throw -> GameState
    add (GameState sc b l) t = GameState { _Score = newScore sc b t, 
                                           _Bonus = newBonus l t,
                                           _Last  = newLast l t}
        where 
        newScore :: Score -> Bonus -> Throw -> Score
        newScore sc False t = sc + t 
        newScore sc True  t = sc + t + t

        newBonus :: Maybe Throw -> Throw -> Bonus
        newBonus l t = fmap (+t) l == Just 10   

        newLast :: Maybe Throw -> Throw -> Maybe Throw
        newLast (Just _) _ = Nothing
        newLast Nothing  t = Just t
    


