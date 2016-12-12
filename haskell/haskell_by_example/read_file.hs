import System.IO

readAtLeast :: Handle -> Integer -> IO String
readAtLeast handle n
    | n <= 0 = return ""
    | otherwise = do
        eof <- hIsEOF handle
        if eof
            then return ""
            else do
              c <- hGetChar handle
              str <- readAtLeast handle (n-1)
              return (c:str)

main = do
    dat <- readFile "/tmp/dat"
    putStrLn dat

    handle <- openFile "/tmp/dat" ReadMode

    b1 <- readAtLeast handle 5
    putStrLn $ show (length b1) ++ " bytes: " ++ b1

    hSeek handle AbsoluteSeek 6
    at2 <- hTell handle
    b2  <- sequence . take 7 . repeat $ hGetChar handle
    putStrLn $ show (length b2) ++ " bytes @ " ++ show at2 ++  ": " ++ b2

    hSeek handle AbsoluteSeek 6
    at3 <- hTell handle
    b3  <- readAtLeast handle 7
    putStrLn $ show (length b3) ++ " bytes @ " ++ show at2 ++  ": " ++ b3

    hSeek handle AbsoluteSeek 0
    hSetBuffering handle (BlockBuffering Nothing) -- default
    b3 <- readAtLeast handle 5
    putStrLn $ show (length b3) ++ " bytes: " ++ b3

    hClose handle
