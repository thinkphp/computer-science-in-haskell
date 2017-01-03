-- List comprehensions can also use logical expressions called GUARDs to filter the values produced by earlier generators.
-- If a guard is True, then the current values are retained, and, if it is False, then they are discarded.
-- For example, the comprehension [x|x<-[1..10], even x] produces the list [2,4,6,8,10] of all even numbers from the list [1..10]

factors :: Int -> [ Int ]

factors n = [ x | x <- [1..n], n `mod` x == 0 ]

prime :: Int -> Bool

prime n = factors n == [1,n]

primes :: Int -> [ Int ]

primes n = [x| x <- [2..n], prime x]

main = do putStrLn "Give me a number"

          n <- readLn :: IO Int 

          print $ prime n

          print $ primes 100
