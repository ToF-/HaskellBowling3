module Bowling
where

type GameState = Int
type Throw = Int

score :: [Throw] -> Int
score = getScore . foldl add initial 
    where
    initial :: GameState 
    initial = 0

    add :: GameState -> Throw -> GameState
    add = (+)

    getScore :: GameState -> Int
    getScore = id
