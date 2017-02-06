import Test.Hspec
import Bowling

main = hspec $ do
    describe "score" $ do
        describe "should calculate bowling scores" $ do
            it "games with no special points" $ do
                score [3,6,6,3,5,4,4,5,1,8,8,1,9,0,0,9,2,7,7,2] `shouldBe` 90
