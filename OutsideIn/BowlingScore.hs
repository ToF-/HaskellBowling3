module BowlingScore where

type Score = Int

bowlingScore :: Monad m => Score -> (m String) -> (String -> m ()) -> m Score 
bowlingScore score inp out = 
    inp >>= \line -> case words line of 
        ["throw",pins] -> (out $ "Score = " ++ (show (score + (read pins)))) >> return (score + (read pins))
