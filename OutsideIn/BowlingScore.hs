module BowlingScore where

type Score = Int

bowlingScore :: Monad m => (m String) -> (String -> m ()) -> m () 
bowlingScore inp out = 
    inp >>= \ls -> out (unlines (snd (foldl score (0,[]) (lines ls))))  

    where 
    score (sc,o) s = case words s of
        ["throw", pins] -> (sc', o ++ ["Score = " ++ show sc'])
            where
            sc' = sc + (read pins)
    
