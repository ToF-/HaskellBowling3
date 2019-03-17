import Test.Hspec
import Bowling

main = hspec $ do
    describe "bowling score calculates current score" $ do
        let current = fst . score
        it "for a game not started" $ do
            current [] `shouldBe` 0 
        it "for a game with only regular frames" $ do
            current [0,4,3,4] `shouldBe` 11
        it "for a game with spares" $ do
            current [4,6,3,4] `shouldBe` 20
        it "for a game with strikes" $ do
            current [10,6,3] `shouldBe` 28
        it "for a game with final frame" $ do
            current (replicate 12 10) `shouldBe` 300
    describe "bowling score calculates maximum score" $ do
        let maxScore = snd . score
        it "for a game not started" $ do
            maxScore [] `shouldBe` 300
        it "for a game with only one regular throw" $ do
            maxScore [1] `shouldBe` 290
            maxScore [2] `shouldBe` 290
        it "for a game with only one strike" $ do
            maxScore [10] `shouldBe` 300
        it "for a game with several regular frames" $ do
            maxScore [3,4,3,4] `shouldBe` 254
        it "for a game with a frame not done yet" $ do
            maxScore [3,4,4] `shouldBe` 267
    it "bowling score calculates both" $ do
            score [10,10,10,10,10,10,10,10,10,7] `shouldBe` (254,277)
            score []   `shouldBe` (0,300)
            score [1]  `shouldBe` (1,290) 
            score [4,5]`shouldBe` (9,279)
            score [4,6]`shouldBe` (10,290)
            score [4,6,1]`shouldBe`(12,271)
            score [4,6,5]`shouldBe`(20,275)
            score [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] `shouldBe` (0,30)
            score [10,10,10,10,10,10,10,10,10] `shouldBe` (230,300)
            score [5,3,7,3,6,2,10,7,2] `shouldBe` (60,210)


