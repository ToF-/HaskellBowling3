import BowlingScore

process :: Score -> IO ()
process score = bowlingScore score getLine putStrLn 
            >>= \score' -> process score' 

main = process 0
