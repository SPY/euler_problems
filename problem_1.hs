modBy n = (== 0) . flip mod n

answer = sum $ filter (\n -> modBy 3 n || modBy 5 n) $ [1..999]