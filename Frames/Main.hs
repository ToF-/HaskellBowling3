import Bowling

main = interact  (unlines . map (show . score . read) . lines)  
