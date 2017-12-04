import Test.Hspec
import Control.Monad.Writer (writer, runWriter)
import BowlingScore

out = \s -> writer ((),s)


main = hspec $ do 
    describe "bowlingScore" $ do
        describe "gets a command and puts a response to it" $ do
            it "S <n> spots a number of pins, add them then prompts the score" $ do

                let
                    inp = return "throw 7"
                    initial = 1
                    run = bowlingScore initial inp out

                snd (runWriter run)  `shouldBe` "Score = 8"

                let
                    inp = return "throw 7"
                    initial = 2
                    run = bowlingScore initial inp out

                snd (runWriter run)  `shouldBe` "Score = 9"

                
