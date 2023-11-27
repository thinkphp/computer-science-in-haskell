module Main where
 
readInts :: IO [Int]
readInts = fmap (map read.words) getLine
 
--main--
main::IO()
main = do   
        line <- readInts 
        let a = line !! 0
        let b = line !! 1
        let c = line !! 2
        let delta = b ^ 2 - 4*a*c
        let x_1 = (fromIntegral(-b) - sqrt(fromIntegral delta))/ fromIntegral(2*a)
        let x_2 = (fromIntegral(-b) + sqrt(fromIntegral delta))/ fromIntegral(2*a)
        
        if x_1 > x_2
	   then do print x_1;
	           print x_2;
           else do print x_2;
                   print x_1
