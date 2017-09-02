import Test.Hspec
import Test.QuickCheck
import qualified Recursive.Bowling as R

data Normal = Normal [Int]
    deriving (Show)

instance Arbitrary Normal where
    arbitrary = do
        n  <- elements [0..20]
        ts <- vector n
        return $ Normal $ map (`mod` 5) ts

main = hspec $ do
    describe "score" $ do
        describe "for poor performance players" $ do
            it "should equal the sum of the throws" $ property $ do
                \(Normal ts) -> R.score ts == sum ts
