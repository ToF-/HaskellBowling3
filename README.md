# HaskellBowling3

3 implementations ideas for the bowling score kata:

- pattern matching and recursion over throws and frame count
- folding throws into a game state
- calculating the list of throws indices to be counted as score

## 1 pattern matching and recursion

## 2 folding throws into a game state

## 3 enumerating throws indices to be added

Example: `[6,4,10,0,10,2,4]

- index the list: [(0,6),(1,4),(2,10),(3,0),(4,10),(5,2),(6,4)]
- starting with a state of (0, Nothing, []), we process each 


- we start with (0,Nothing,[]) and receive (0,6) : add 0 to the list, current frame score is Nothing, and throw is 6 and not 10 so mark the current frame score at Just 6
- we have (0,Just 6,[0]) and receive (1,4) : add 1 to the list, current frame score is not Nothing so add throw to it. it is now Just 10 : it's a spare so add 2 to the list, and mark current frame score at Nothing and increment frame count
- we have (1,Nothing,[0,1,2]) and receive (2,10) : add 2 to the list, current frame score is Nothing, and throw is 10, so add 3 and 4 to the list, and mark current frame score at Nothing, and increment frame count
- we have (2,Nothing,[0,1,2,2,3,4]) and receive (3,0) : add 3 to the list, current frame score is Nothing and throw is 0 and not 10 so mark the current frame score at Just 0
- we have (2,Just 0,[0,1,2,2,3,4,3]) and receive (4,10) : add 4 to the list, current frame score is not nothing so add throw to it, is is now Just 10, it's a spare so add 5 to the list

