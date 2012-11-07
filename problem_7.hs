divisor n = (== 0) . mod n

isPrime n = (1 ==) . length $ filter (divisor n) $ [1..(floor . sqrt . fromIntegral $ n )]

answer = filter isPrime (2 : [3,5..]) !! 10000
