module Main where
 
import Text.Printf
import Control.Monad
import Data.List
 
 
run :: [Double] -> (Int, [Double])
run [a,b,c] = 
    let d = b^2 - 4*a*c 
    in case d of 
       _ | abs a < 1e-308 -> if abs b > 1e-308 then (1,[-c/b]) else (if c/=0 then 0 else -1,[]) 
       _ | d < 0 -> (0, []) 
       _ | d == 0 -> let root = (-b)/(2*a) in  (1, [if root == 0 then 0 else root])
       _  -> let roots = map (\d -> ((-b) + d)/(2*a)) $ zipWith (*) [1, (-1)] $ map sqrt [d,d] 
             in (2,  sort roots)
       
 
main =  do
    (n, roots) <- getLine >>= return . run . map (\x -> fromIntegral $ (read x::Int)) . take 3 . words
    print n
    mapM_ (printf "%.15f\n") roots
