import Test.Hspec
import Test.QuickCheck
import qualified Recursive.Bowling as R

data Normal = Normal [Int]
    deriving (Show)

poor = map (`mod` 5)

instance Arbitrary Normal where
    arbitrary = do
        n  <- elements [0..20]
        ts <- vector n
        return $ Normal $ poor ts

data Spare1 = Spare1 [Int]
    deriving (Show)

instance Arbitrary Spare1 where
    arbitrary = do
        n <-  elements [1..18]
        ts <- fmap poor $ vector n 
        t <- elements [0..9]
        let u = 10 - t
        return $ Spare1 $ t:u:ts

data Spares = Spares [Int]
    deriving (Show)

spare x = [t,u] 
    where
    t = x `mod` 10
    u = 10 - t

instance Arbitrary Spares where
    arbitrary = do
        fs <- vector 9
        let ts = concatMap spare fs
        end <- fmap poor $ vector 2
        return $ Spares $ ts ++ end

        
main = hspec $ do
    describe "score" $ do
        describe "for games without any spare or strike" $ do
            it "should equal the sum of the throws" $ verbose.property $ do
                \(Normal ts) -> R.score ts == sum ts
        describe "for games with a spare at first frame" $ do
            it "should add the next throw as a bonus" $ verbose.property $ do
                \(Spare1 ts) -> R.score ts == sum ts + ts!!2
        describe "for games with spares in all frames except last" $ do
            it "should add the next throw as a bonus" $ verbose.property $ do
                \(Spares ts) -> R.score ts == sum ts + sum (map (ts!!) [2,4..18])
        describe "for games with strikes in all frames except last" $ do
            it "should add the two next throws as bonuses" $ do
                R.score ((replicate 9 10)++[3,3]) `shouldBe` 255
        describe "for games with spares in the end" $ do
            it "should take supplementary throw only as a bonus" $ verbose.property $ do
                \(Spares sps) (Spare1 end) -> let game = take 18 sps ++ end 
                                            in R.score game == R.score (take 20 game) + game!!20
        describe "for games with strikes in the end" $ do
            it "should take supplementary throws only as a bonus" $ do
                R.score (replicate 12 10)  `shouldBe` 300
