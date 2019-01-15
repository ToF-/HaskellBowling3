import Test.QuickCheck

quickCheckOf :: Testable a => String -> a -> IO ()
quickCheckOf s p = putStrLn (s ++ "\t") >> quickCheck p

prop_dummy :: Int -> Bool
prop_dummy x = True

main = do
    quickCheckOf "dummy property" prop_dummy
