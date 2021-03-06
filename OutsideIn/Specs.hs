import Test.Hspec
import Control.Monad.Writer (writer, runWriter)
import BowlingScore

out = \s -> writer ((),s)


main = hspec $ do 
    describe "bowlingScore" $ do
        describe "gets a command and puts a response to it" $ do
            it "S <n> spots a number of pins, add them then prompts the score" $ do

                let
                    inp = return $ unlines ["throw 7",
                                            "throw 2"]
                    run = bowlingScore inp out

                (lines.snd) (runWriter run)  `shouldBe` ["Score = 7"
                                                        ,"Score = 9"]


                
