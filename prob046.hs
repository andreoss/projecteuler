square x = x * x
double   = (* 2)
  
isPrime :: Integral a => a -> Bool
isPrime n
  | n == 1     = False
  | n == 2     = True
  | even n    = False
  | otherwise = null $ filter (\ x -> rem n x == 0) [3 .. l]
  		where l = round $ sqrt $ fromIntegral n

primes  = filter isPrime [1..]
doubleSquares = map (double . square) [1..]

isGoldbach' n prime
  | prime >= n = False
  | otherwise  = not . null
                 $ filter (\s ->  n == prime + s )
                 $ takeWhile (< n) doubleSquares

isGoldbach n = not . null
             $ dropWhile (not . isGoldbach' n)
             $ takeWhile (< n) primes

result  = head
          $ dropWhile isGoldbach
          $ filter (not . isPrime)
          $ filter odd [3..] 
          

main    = do print result
