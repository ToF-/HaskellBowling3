import Test.Hspec
import Bowling

main = hspec $ do
    describe "bowling score calculates current and max score" $ do
        it "for a game not started" $ do
            score [] `shouldBe` (0,300)

        it "for a game with 1 regular throw" $ do
            score [1] `shouldBe` (1,290)
