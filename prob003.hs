isPrime :: Integral a => a -> Bool
isPrime n
  | n == 1     = False
  | n == 2     = True
  | even n    = False
  | otherwise = (not . any (\ x -> rem n x == 0)) [3 .. l]
  		where l = round $ sqrt $ fromIntegral n

nthPrime :: Integral a => Int -> a
nthPrime = (!!) $ filter isPrime [1..]

factors' :: Integral a => a -> Int -> [a] -> [a]
factors' n primeN acc 
  | n == 1 = acc
  | otherwise = let prime = nthPrime primeN
                in if rem n prime == 0 then
                   factors' (quot n prime) primeN (prime:acc)
                 else
                   factors' n (primeN + 1) acc
                   
factors n = factors' n 0 []

answer = maximum $ factors 600851475143 
main   = print answer
