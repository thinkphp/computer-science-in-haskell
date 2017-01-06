gdc :: Int -> Int -> Int

gdc x y | x == y = x
        | x > y = gdc (x - y) y
        | y > x = gdc y (y - x)
main = do 
       print $ gdc 488 240