# Table of Contents: Introduction to Haskell - Stanford Class Coursework

## 0. History of Haskell
- Origins and development
- Key milestones and versions
- Influence on other languages

## 1. Basic Syntax and Function Definition
- Simple functions
- Multiple parameters
- Using guards
- Let expressions

## 2. Lists and List Comprehensions
- Basic list operations
- Simple list comprehensions
- Nested list comprehensions
- List comprehensions with multiple generators

## 3. Pattern Matching
- Pattern matching on lists
- Pattern matching with tuples
- Pattern matching in function definitions
- As-patterns

## 4. Higher-Order Functions
- Map function
- Filter function
- Creating higher-order functions
- Function composition

## 5. Algebraic Data Types (ADTs)
- Simple ADTs
- ADTs with parameters
- Recursive ADTs
- Parametric polymorphism with ADTs

## 6. Type Classes
- Defining a type class
- Implementing a type class
- Using type class constraints
- Multiple type class constraints


# Introduction to Haskell - Stanford Class Coursework

Welcome to the introductory Haskell coursework for Stanford students. This guide covers fundamental concepts in Haskell programming, with a focus on practical examples to reinforce your understanding. We'll start with a brief history of the language to provide context for its development and design principles.

## 0. History of Haskell

Haskell is a purely functional programming language that was first developed in the late 1980s. Here are some key points in its history:

- **1987**: The idea for Haskell was born at the conference on Functional Programming Languages and Computer Architecture (FPCA) in Portland, Oregon. A committee was formed to design a new language that would unify existing functional languages.

- **1990**: The first version of Haskell (Haskell 1.0) was released. It was named after the logician Haskell Curry, known for his work in combinatory logic.

- **1998**: Haskell 98 was released, which became the first stable and widely-used version of the language. It provided a standard to ensure portability of Haskell code across implementations.

- **2010**: Haskell 2010 was released, introducing several new features including the Foreign Function Interface (FFI) for calling non-Haskell functions, a hierarchical module system, and more.

- **Present**: Haskell continues to evolve, with the Glasgow Haskell Compiler (GHC) being the most widely used implementation. It's known for its strong type system, lazy evaluation, and emphasis on pure functions.

Haskell's design was influenced by languages like Miranda, and it has in turn influenced many other languages, including F#, Scala, and Rust.

## 1. Basic Syntax and Function Definition

In Haskell, functions are the primary building blocks of your program. Let's look at some examples:

### Example 1: Simple function
```haskell
double :: Int -> Int
double x = x * 2
```
This function takes an Int and returns an Int. The `::` symbol is read as "has type of".

### Example 2: Multiple parameters
```haskell
add :: Int -> Int -> Int
add x y = x + y
```
In Haskell, functions are curried by default. This means `add` can be partially applied:
```haskell
addFive :: Int -> Int
addFive = add 5
```

### Example 3: Using guards
```haskell
absoluteValue :: Int -> Int
absoluteValue n
    | n < 0     = -n
    | otherwise = n
```
Guards allow you to define functions piecewise, based on conditions.

### Example 4: Let expressions
```haskell
cylinderSurfaceArea :: Float -> Float -> Float
cylinderSurfaceArea radius height =
    let sideArea = 2 * pi * radius * height
        topArea = pi * radius^2
    in  sideArea + 2 * topArea
```
Let expressions allow you to define local variables within a function.

## 2. Lists and List Comprehensions

Lists are a fundamental data structure in Haskell. List comprehensions provide a concise way to create lists based on existing lists.

### Example 1: Basic list operations
```haskell
numbers = [1, 2, 3, 4, 5]
doubledNumbers = map (*2) numbers  -- [2, 4, 6, 8, 10]
sumOfNumbers = sum numbers  -- 15
```

### Example 2: List comprehension
```haskell
evens = [x | x <- [1..10], even x]  -- [2, 4, 6, 8, 10]
```

### Example 3: Nested list comprehension
```haskell
matrix = [[1,2,3], [4,5,6], [7,8,9]]
flattenedMatrix = [x | row <- matrix, x <- row]  -- [1,2,3,4,5,6,7,8,9]
```

### Example 4: List comprehension with multiple generators
```haskell
pythagoreanTriples = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
-- [(3,4,5), (6,8,10)]
```
This generates all Pythagorean triples where each component is less than or equal to 10.

## 3. Pattern Matching

Pattern matching is a powerful feature in Haskell that allows you to destructure data and define function behavior based on input patterns.

### Example 1: Pattern matching on lists
```haskell
head' :: [a] -> a
head' []    = error "Empty list"
head' (x:_) = x

tail' :: [a] -> [a]
tail' []     = error "Empty list"
tail' (_:xs) = xs
```

### Example 2: Pattern matching with tuples
```haskell
fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y
```

### Example 3: Pattern matching in function definitions
```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
```

### Example 4: Pattern matching with as-patterns
```haskell
firstLetter :: String -> String
firstLetter ""    = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
```
As-patterns allow you to pattern match and keep a reference to the entire value.

## 4. Higher-Order Functions

Haskell treats functions as first-class citizens, allowing you to pass functions as arguments and return them as results.

### Example 1: map function
```haskell
doubleList :: [Int] -> [Int]
doubleList = map (*2)

squareList :: [Int] -> [Int]
squareList = map (^2)
```

### Example 2: filter function
```haskell
evenNumbers :: [Int] -> [Int]
evenNumbers = filter even

positiveNumbers :: [Int] -> [Int]
positiveNumbers = filter (>0)
```

### Example 3: Creating a higher-order function
```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- Usage: applyTwice (*2) 3 results in 12

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 f x = x
applyNTimes n f x = f (applyNTimes (n-1) f x)
-- Usage: applyNTimes 3 (*2) 2 results in 16
```

### Example 4: Function composition
```haskell
oddSquareSum :: Integer -> Integer
oddSquareSum = sum . map (^2) . filter odd . takeWhile (<10000)
-- This function sums the squares of all odd numbers less than 10000
```

## 5. Algebraic Data Types

Algebraic Data Types (ADTs) allow you to create custom types in Haskell.

### Example 1: Simple ADT
```haskell
data Bool = False | True

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
```

### Example 2: ADT with parameters
```haskell
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle w h) = w * h
```

### Example 3: Recursive ADT
```haskell
data List a = Empty | Cons a (List a)

length' :: List a -> Int
length' Empty = 0
length' (Cons _ xs) = 1 + length' xs
```

### Example 4: Parametric polymorphism with ADTs
```haskell
data Maybe a = Nothing | Just a

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
```

## 6. Type Classes

Type classes provide a way to define generic interfaces that can be implemented by multiple types.

### Example 1: Defining a type class
```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)
```

### Example 2: Implementing a type class
```haskell
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
```

### Example 3: Using a type class constraint
```haskell
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = x == y || elem x ys
```

### Example 4: Multiple type class constraints
```haskell
sortAndShow :: (Ord a, Show a) => [a] -> String
sortAndShow xs = show (sort xs)
```

This introduction covers fundamental concepts in Haskell, providing multiple examples for each concept to reinforce your understanding. As you progress through the course, you'll dive deeper into these concepts and explore more advanced features of the Haskell language, such as monads, applicatives, and advanced type system features.

Remember that Haskell's strength lies in its purity, strong static typing, and lazy evaluation. These features allow for writing concise, modular, and efficient code, especially for complex systems and mathematical computations.

Happy Haskelling!


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

https://cs.lmu.edu/~ray/notes/introhaskell/

https://www.cantab.net/users/antoni.diller/haskell/haskell.html
* https://www.sfu.ca/~tjd/383summer2019/haskell_folding_lhs.html Folding
* https://stackoverflow.com/questions/1757740/how-does-foldr-work Folding
