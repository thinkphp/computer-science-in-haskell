fact :: Integer -> Integer

fact n = if n == 0 then 1 else n * fact(n - 1)

main = do 

       putStrLn "n <- " 

       n <- readLn :: IO Integer

       print $ fact n 
