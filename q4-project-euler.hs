main = print $ maximum [x | a <- [100..999], b <- [a..999], let x = a*b, let s = show x, s == reverse s]
