is_poly n = even len && l == (reverse r)
	where s = show n
	      len = length s
	      (l, r) = splitAt (len `div` 2) s 

range = [999,998..100]

answer = maximum $ filter is_poly $ [ x * y | x <- range, y <- range ]

	    