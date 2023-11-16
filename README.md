# Essential Haskell

![Haskell logo](https://raw.githubusercontent.com/abrahamcalf/programming-languages-logos/master/src/haskell/haskell.svg)

## Intro

```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
add :: Integer -> Integer -> Integer
sayMe :: Integer -> String
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

## List comprehensions

list comprehension returns a list of elements created by evaluation of the generators

```haskell
Input: [x+2*x+x/2 | x <- [1,2,3,4]]
Output: [3.5,7.0,10.5,14.0]

Input: [ odd x | x <- [1..9]]
Output: [True,False,True,False,True,False,True,False,True]

Input: [ x*y | x <- [1,2,3,4], y <- [3,5,7,9]]
Output: [3,5,7,9,6,10,14,18,9,15,21,27,12,20,28,36]

Input: [x | x <- [1,5,12,3,23,11,7,2], x>10]
Output: [12,23,11]

Input: [(x,y) | x <- [1,3,5], y <- [2,4,6], x<y]
Output: [(1,2),(1,4),(1,6),(3,4),(3,6),(5,6)]
```
## Foldr

it takes the second argument and the last item of the list and applies the function, then it takes the penultimate item from the end and the result, and so on. See scanr for intermediate results

```
Input: foldr (+) 5 [1,2,3,4]
Output: 15

Input: foldr (/) 2 [8,12,24,4]
Output: 8.0

Input: foldr (/) 3 []
Output: 3.0

Input: foldr (&&) True [1>2,3>2,5==5]
Output: False

Input: foldr max 18 [3,6,12,4,55,11]
Output: 55

Input: foldr (\x y -> (x+y)/2) 54 [12,4,10,6]
Output: 12.0
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

## Fibonacci Sequence

```haskell
hello = "Fibonacci Sequence!"
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib ( n - 2)
main :: IO()
main = do print(hello)
          print(fib 10)
```

## Square https://ideone.com/mVdZqO

```Haskell
square :: Int -> Int
square n = n * n
main = do 
       print $ square 5
```

## References

https://www.cantab.net/users/antoni.diller/haskell/haskell.html
* https://www.sfu.ca/~tjd/383summer2019/haskell_folding_lhs.html Folding
* https://stackoverflow.com/questions/1757740/how-does-foldr-work Folding
