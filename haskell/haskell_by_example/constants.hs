s :: String
s = "constant"

main = do
  putStrLn s

  let n = 500000000
  let d = 3e30 / n

  print d
  print $ sin n
