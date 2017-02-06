import Test.Hspec
import Bowling

main = hspec $ do
    describe "score" $ do
        describe "should calculate bowling score" $ do
            it "for games with no special points" $ do
                score [3,6,6,3,5,4,4,5,1,8,8,1,9,0,0,9,2,7,7,2] `shouldBe` 90
                score [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] `shouldBe` 0
            it "for games with spares" $ do
                score [5,5,5,5] `shouldBe` 25
                score [8,2,1,9,4,4] `shouldBe` 33
            it "for games with strikes" $ do
                score [10,4,3] `shouldBe` 24
