import Test.QuickCheck

type Throw = Integer

score :: [Throw] -> Integer
score = sum
data Frame = Regular Throw Throw    
    deriving (Eq,Show)

allThrows :: [Frame] -> [Throw]
allThrows = concatMap throws 
    where
    throws :: Frame -> [Throw]
    throws (Regular a b) = [a,b]


    
regular :: Gen Frame 
regular = do
        a <- choose (0,9)
        b <- choose (0,9-a)
        return $ Regular a b


quickCheckOf :: Testable a => String -> a -> IO ()
quickCheckOf s p = putStr (s ++ "\t") >> quickCheck p

prop_dummy :: Int -> Bool
prop_dummy x = True

regulars :: Gen [Frame] 
regulars = listOf regular

prop_regular :: Property
prop_regular = forAll regulars $ \fs -> let ts = allThrows fs in score ts == sum ts

main = do
    quickCheckOf "dummy property" prop_dummy
    quickCheckOf "regulars score as sum of throws" prop_regular
