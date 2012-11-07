bigNum = 600851475143

divisor n = (== 0) . mod n

isPrime n = (1 ==) . length $ filter (divisor n) $ [1..(floor . sqrt . fromIntegral $ n )]

iterator (d, n) (x: xs) 
	     	  | x > n = d
	     	  | (divisor n x) && (isPrime x) = iterator ( max d x, n `div` x) xs
		  | otherwise = iterator (d, n) xs

answer = iterator (1, bigNum) [1,3..]