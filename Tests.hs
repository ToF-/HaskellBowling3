import Test.QuickCheck
import qualified Recursive.Bowling as R
import qualified GameState.Bowling as G
import qualified Frames.Bowling as F


arbitraryThrow :: Gen R.Throw
arbitraryThrow = choose (0,10)

arbitraryFrame :: Gen [R.Throw]
arbitraryFrame = do
    x <- arbitraryThrow 
    y <- choose (0,10-x)
    return $ if x == 10 then [x] else [x,y]

arbitraryGame :: Gen (Int,[R.Throw])
arbitraryGame = do
    frames <- sequence [arbitraryFrame | _ <- [1..10]]
    final <- case last frames of
        [10] -> sequence [arbitraryThrow,arbitraryThrow]
        [x,y] -> if x+y == 10 then fmap (take 1) arbitraryFrame 
                              else return []
    let throws = concat frames ++ final 
    return $ (R.score throws,throws)  

recursiveScore = R.score
gameStateScore = G.score
frameMarkScore = F.score

scoring = forAll arbitraryGame $ 
    \(result,throws) -> gameStateScore throws == result 
                     && frameMarkScore throws == result 

main = verboseCheck scoring

