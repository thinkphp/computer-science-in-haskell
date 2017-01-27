fact :: Integer -> Integer

fact 0 = 1

fact n |  n > 1 = n * fact( n - 1)


main = do 

       putStrLn "n <- " 

       n <- readLn :: IO Integer

       print $ fact n 
