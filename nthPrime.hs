-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

-- What is the 10001st prime number?

import Data.List

square n = n * n

divideBy n x = (x `mod` n) == 0

isPrime n | n <= 1 = False
          | n == 2 = True
          | n == 3 = True
          | even n = False
          | otherwise = 
            all(\p -> not $ divideBy p n) testingSlice

               where 
                   testingNumbers = iterate (+ 2) 3         
                   testingSlice = takeWhile(\p -> square p <= n) testingNumbers 

nextPrime n = 

    let Just m = find isPrime [n+1..]
    in m 

allPrimes = 

    iterate nextPrime 2

main = do    
       print $ last $ take 10001 allPrimes