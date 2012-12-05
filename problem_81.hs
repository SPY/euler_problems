import System.Environment ( getArgs )
import Data.List.Split (splitOn)

calc :: [[Int]] -> Int -> Int
calc ns n = ans !! n !! n
          where ans = map (\x -> map (\y -> (calcFor x y)) [0..]) [0..]
                calcFor 0 0 = ns !! 0 !! 0
                calcFor 0 y = (ns !! 0 !! y) + (ans !! 0 !! (y - 1))
                calcFor x 0 = (ns !! x !! 0) + (ans !! (x - 1) !! 0)
                calcFor x y = (ns !! x !! y) + min (ans !! (x - 1) !! y) (ans !! x !! (y - 1))

main :: IO ()
main = do
     [fileName] <- getArgs
     cnt <- readFile fileName
     let nums = map (map read) . map (splitOn ",") . lines $ cnt
         n = (length nums) - 1 
     putStrLn . show $ calc nums n
     
     