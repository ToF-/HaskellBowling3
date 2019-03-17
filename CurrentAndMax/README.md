
Given a list of bowling throws, calculate the current score and the maximum possible score.

The score should be calculated even when the list of throws doesn't form a complete game.

For example the list `[4,6]` gives a score ≥ 10.

Examples:

```haskell
score []   == (0,300)
score [1]  == (1,290) 
score [4,5]== (9,279)
score [4,6]== (10,290)
score [4,6,1]==(11,271)
score [4,6,5]==(15,275)
score [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] == (0,30)
score [10,10,10,10,10,10,10,10,10] == (210,300)
score [5,3,7,3,6,2,10,7,2] == (60,210)
```



