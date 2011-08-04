import Data.List
import Data.Char (digitToInt)


-- isPrime :: Integral a => a -> Bool
-- isPrime n
--   | n == 1     = False
--   | n == 2     = True
--   | even n    = False
--   | otherwise = null $ filter (\ x -> rem n x == 0) [3 .. l]
--   		where l = round $ sqrt $ fromIntegral n

noDivs n fs = foldr (\f r -> f*f > n || (rem n f /= 0 && r)) True fs
-- primes = filter (`noDivs`[2..]) [2..]
primesTD = 2 : 3 : filter (`noDivs` tail primesTD) [5,7..]
isPrime n = n > 1 && noDivs n primesTD

isPandigital n = sort ds == [1..(length ds)]
  where ds = map digitToInt $ show n

primes = filter isPrime [987654312, 987654311 .. 1]

main = do print (head primes)
