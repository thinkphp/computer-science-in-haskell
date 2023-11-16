# Essential Haskell

![Haskell logo](https://raw.githubusercontent.com/abrahamcalf/programming-languages-logos/master/src/haskell/haskell.svg)

## Intro

```haskell
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe 6 = "Six"
sayMe 7 = "Seven"
sayMe 8 = "eight"
sayMe 9 = "Nine"
sayMe 10 = "Ten"
add x y = x + y
main :: IO()
main = do
       putStr "Sum of x + y = "
       print(add 10 10)
       print(sayMe 10) 
       print $ factorial 10 
       print(factorial 5)
```

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

## Factorial https://ideone.com/GwJc9z

```haskell
GNU nano 6.2                         Fact.hs                            M     
fact :: Integer -> Integer
fact n = if n == 0 then 1 else n * fact (n - 1)

main = do 
       putStrLn "n <- "
       n <- readLn :: IO Integer
       print $ fact n

```

## Square https://ideone.com/mVdZqO

```Haskell
square :: Int -> Int
square n = n * n
main = do 
       print $ square 5
```
