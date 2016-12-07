import System.Environment

main = do
    setEnv "FOO" "1"
    putStr "FOO:" >> (putStrLn  =<< getEnv "FOO")
    putStr "BAR:" >> (print  =<< lookupEnv "BAR")
    putStrLn ""
    mapM_ putStrLn =<< map fst `fmap` getEnvironment
