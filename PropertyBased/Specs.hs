import Test.QuickCheck

type Throw = Integer

score :: [Throw] -> Integer
score = score' 0
score' :: Integer -> [Throw] -> Integer
score' 10 _ = 0
score' f [] = 0
score' f (10:y:z:xs) = 10 + y + z + score' (f+1) (y:z:xs)
score' f (x:y:z:xs) | x + y == 10 = 10 + z + score' (f+1) (z:xs)
                    | otherwise = x + y + score' (f+1) (z:xs)
score' f (10:y:xs) = 10 + y + score' (f+1) (y:xs)
score' f (x:y:xs) = x + y + score' (f+1) xs
score' f (x:xs) = x + score' f xs

data Frame = Regular Throw Throw    
           | Spare Throw
           | Strike
    deriving (Eq,Show)

allThrows :: [Frame] -> [Throw]
allThrows = concatMap throws 
    where
    throws :: Frame -> [Throw]
    throws (Regular t u) = [t,u]
    throws (Spare t) = [t,10-t]
    throws Strike = [10]


    
regular :: Gen Frame 
regular = do
        t <- choose (0,9)
        u <- choose (0,9-t)
        return $ Regular t u

spare :: Gen Frame
spare = do
        t <- choose (0,9)
        return $ Spare t

strike :: Gen Frame
strike = return Strike

quickCheckOf :: Testable a => String -> a -> IO ()
quickCheckOf s p = putStr (s ++ "\t") >> quickCheck p

regulars :: Gen [Frame] 
regulars = fmap (take 10) $ listOf regular

spares :: Gen [Frame]
spares = fmap (take 10) $ listOf spare

strikes :: Gen [Frame]
strikes = fmap (take 10) $ listOf strike

frame :: Gen Frame
frame = do
    t <- choose (0,10)
    u <- choose (0,10-t)
    return $ fromThrows t u
    where
    fromThrows 10 0 = Strike
    fromThrows t u | t + u == 10 = Spare t
                   | otherwise = Regular t u

prop_regular :: Property
prop_regular = forAll regulars $ \fs -> let ts = allThrows fs in score ts == sum ts

prop_spare :: Property
prop_spare = forAll spares $ \fs -> let ts = allThrows fs in score ts == (sum ts) + (sum (drop 1 (map (\(Spare n) -> n) fs)))

prop_strike :: Property
prop_strike = forAll strikes$ \fs -> let ts = allThrows fs in score ts == (sum ts) + (sum (drop 1 ts)) + (sum (drop 2 ts))

prop_regular_after_tenth :: Property
prop_regular_after_tenth = forAll (listOf regular) $ \fs -> let ts = allThrows fs in score ts == score (take 20 ts)

prop_maximum_score :: Property
prop_maximum_score = forAll (listOf frame) $ \fs -> let ts = allThrows fs in score ts <= 300
main = do
    quickCheckOf "regulars score as sum of throws" prop_regular
    quickCheckOf "spares score as sum of throws plus bonuses" prop_spare 
    quickCheckOf "strikes score as sum of throw plus double bonuses" prop_strike
    quickCheckOf "regulars after 10th frame don't count" prop_regular_after_tenth
    quickCheckOf "maximum score is 300" prop_maximum_score
