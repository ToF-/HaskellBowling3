module Frames.Bowling
where
import Data.Maybe

type Throw = Int
type Score = Int
data Frame = Fr Int FrameType
    deriving (Eq, Show)
data FrameType = Half Throw | Normal | Spare | Strike | BonusThrow
    deriving (Eq, Show)
    
type Index = Int

score :: [Throw] -> Score
score ts = collect $ indices $ frames $ ts
    where

    collect :: [Index] -> Score
    collect = sum . map (ts!!) 

    frames :: [Throw] -> [Frame]
    frames = tail . scanl next (Fr (-1) Normal)

    next :: Frame -> Throw -> Frame
    next (Fr n (Half p')) p | p + p' == 10 = Fr n Spare
    next (Fr n (Half _ )) _                = Fr n Normal
    next (Fr 9 _) _  = Fr 9 BonusThrow
    next (Fr n _) 10 = Fr (succ n) Strike
    next (Fr n _) p  = Fr (succ n) (Half p) 
                               

    indices :: [Frame] -> [Index]
    indices fs = concat $ zipWith count [0..] fs

    count :: Index -> Frame -> [Index]
    count _ (Fr _ BonusThrow) = []
    count i (Fr _ (Half _))   = [i]
    count i (Fr _ Normal)     = [i]
    count i (Fr _ Spare)      = [i,succ i]
    count i (Fr _ Strike)     = [i,succ i, succ $ succ i]
