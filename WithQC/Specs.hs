import Test.Hspec
import Test.QuickCheck
import Bowling

data TestGame = TG [Int]
    deriving (Eq, Show)

data TestFrame = TF [Int]
    deriving (Eq, Show)

instance Arbitrary TestFrame where
    arbitrary = do
        t1 <- elements [0..10]
        t2 <- elements [(10-t1)..10] 
        return $ case t2 of
            0 -> TF [t1]
            _ -> TF [t1,t2]

instance Arbitrary TestGame where
    arbitrary = do 
        nf <- elements [1.10]
        ts <- throws nf
        return $ TG ts
throws 0 = return []
throws n = do
    TF f <- arbitrary 
    fs  <- throws (n-1)
    return $ fs ++ f

main = hspec $ do
    describe "score" $ do
        it "should compute score for poor performing players" $ property $ do
            \(TG ts) -> score ts == sum ts 
