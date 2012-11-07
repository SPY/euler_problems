
n = 20

solves = map (\x -> map (\y -> (solve x y)) [0..]) [0..]

solve 0 0 = 1
solve 0 y = solves !! 0 !! (y - 1)
solve x 0 = solves !! (x - 1) !! 0
solve x y = (solves !! x !! (y - 1)) + (solves !! (x - 1) !! y)

answer = solve n n



