import Test.Hspec
import Bowling

main = hspec $ do
    describe "bowling score calculates current and max score" $ do
        it "for a game not started" $ do
            score [] `shouldBe` (0,300)
