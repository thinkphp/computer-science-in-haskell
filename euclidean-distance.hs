euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance p q = sqrt $ sum $ zipWith (\ u v -> (u-v)^2) p q

main = do 

     let a = [1,2,3]
     let b = [4,5,6]

     print $ sqrt $ sum $ zipWith (\ u v -> (u-v)^2) a b
     print $ euclideanDistance a b
     print (euclideanDistance a b)



