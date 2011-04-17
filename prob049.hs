
import qualified Data.Set as Set

digits :: Int -> [Int]
digits = map (negate . (-) (fromEnum '0')  . fromEnum) . show 

isPermute a b = Set.fromList ad == Set.fromList bd
                where
                  ad = digits a
                  bd = digits b

isPrime :: Integral a => a -> Bool
isPrime n
  | n == 1     = False
  | n == 2     = True
  | even n    = False
  | otherwise = not $ any (\ x -> rem n x == 0) [3 .. l]
  		where l = round $ sqrt $ fromIntegral n

combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations k (x:xs) = map (x:) (combinations (k - 1) xs) ++
                          combinations k xs

primes   = filter isPrime [1000..9999]

isGood n = filter (\[a,b,c] -> a == n && b == a + 3330 && c == b + 3330 ) $ combinations 3 $ filter (isPermute n) primes

result   = map head $ filter (not . null) $ map isGood primes

answer   = foldr ((++) . show) "" $ result !! 1

main     = putStrLn answer


