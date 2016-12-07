import System.Process
import System.Directory

main = do
    path <- findExecutable "ls"
    case path of
        Nothing -> error "ls doesn't exist"
        Just _  -> createProcess (proc "ls" ["-a", "-l", "-h"])
