import Test.Hspec
import qualified Recursive.Bowling as R
import qualified GameState.Bowling as G
import qualified Frames.Bowling as F

main = hspec $ do
    describe "score" $ do
        describe "should calculate bowling scores" $ do
            it "for games with no special points" $ do
                R.score [3,6,6,3,5,4,4,5,1,8,8,1,9,0,0,9,2,7,7,2] `shouldBe` 90
                R.score [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] `shouldBe` 0
                G.score [3,6,6,3,5,4,4,5,1,8,8,1,9,0,0,9,2,7,7,2] `shouldBe` 90
                G.score [3,6,6,3,5,4,4,5,1,8,8,1,9,0,0,9,2,7,7,2] `shouldBe` 90
                F.score [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] `shouldBe` 0
                F.score [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] `shouldBe` 0
            it "for games with spares" $ do
                R.score [5,5,5,0] `shouldBe` 20
                R.score [8,2,1,9,4,4] `shouldBe` 33
                G.score [5,5,5,0] `shouldBe` 20
                G.score [8,2,1,9,4,4] `shouldBe` 33
                F.score [5,5,5,0] `shouldBe` 20
                F.score [8,2,1,9,4,4] `shouldBe` 33
            it "for games with strikes" $ do
                R.score [10,4,3] `shouldBe` 24
                R.score [10,10,5,5,5] `shouldBe` 65 
                G.score [10,4,3] `shouldBe` 24
                G.score [10,10,5,5,5] `shouldBe` 65 
                F.score [10,4,3] `shouldBe` 24
                F.score [10,10,5,5,5] `shouldBe` 65 
            it "for games with end bonus" $ do
                R.score [10,10,10,10,10,10,10,10,10,10,10,10] `shouldBe` 300
                R.score [10,10,10,10,10,10,10,10,10,6,4,10] `shouldBe` 276
                R.score [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,6,4] `shouldBe` 20
                G.score [10,10,10,10,10,10,10,10,10,10,10,10] `shouldBe` 300
                G.score [10,10,10,10,10,10,10,10,10,10,10,10] `shouldBe` 300
                G.score [10,10,10,10,10,10,10,10,10,6,4,10] `shouldBe` 276
                F.score [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,6,4] `shouldBe` 20
                F.score [10,10,10,10,10,10,10,10,10,6,4,10] `shouldBe` 276
                F.score [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,6,4] `shouldBe` 20
            it "for normal games" $ do
                R.score [6,4,9,1,10,10,10,8,0,2,3,0,10,7,3,5,5,6] `shouldBe` 176
                G.score [6,4,9,1,10,10,10,8,0,2,3,0,10,7,3,5,5,6] `shouldBe` 176
                F.score [6,4,9,1,10,10,10,8,0,2,3,0,10,7,3,5,5,6] `shouldBe` 176


