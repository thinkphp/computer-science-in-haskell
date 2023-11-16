# Essential Haskell

![Haskell logo](https://raw.githubusercontent.com/abrahamcalf/programming-languages-logos/master/src/haskell/haskell.svg)

## Euclid's algorithm https://ideone.com/jJFGHX

```haskell
euclid :: Int -> Int -> Int
euclid a b | a <= 0 && b <= 0 = error "GCD works for Numbers"
           | a == b = a
           | a > b = euclid (a - b) b
           | otherwise = euclid a (b - a)
main :: IO()
main = do putStrLn "Enter the first number:"
          a <- getLine
          let a' = read a :: Int
          putStrLn "Enter the second number:"
          b <- getLine
          let b' = read b :: Int
          putStrLn $ "The GCD of " ++ a ++ " and " ++ b ++ " is " ++ show (euclid a' b')

```

## Factorial

```haskell
GNU nano 6.2                         Fact.hs                            M     
fact :: Integer -> Integer
fact n = if n == 0 then 1 else n * fact (n - 1)

main = do 
       putStrLn "n <- "
       n <- readLn :: IO Integer
       print $ fact n


```
