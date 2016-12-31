main = do
putStrLn "n <--"
n <- readLn :: IO Integer
print $ sum $ map(\x -> x ^ 1) [1..n]

