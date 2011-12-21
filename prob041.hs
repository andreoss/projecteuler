import Data.List (nub)
import Data.Char (digitToInt)
import qualified Data.Set as S


merge :: (Ord a) => [a] -> [a] -> [a]
merge []        ys        = ys
merge xs        []        = xs
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : merge xt ys
    EQ -> x : merge xt yt
    GT -> y : merge xs yt


diff :: (Ord a) => [a] -> [a] -> [a]
diff []         ys       =  ys
diff xs         []       =  xs
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : diff xt ys
    EQ -> diff xt yt
    GT -> diff xs yt

primes, nonprimes :: [Integer]
primes    = [2, 3, 5] ++ diff [7, 9 ..] nonprimes
nonprimes = foldr1 f $ map multiplicative $ tail primes
  where f (x:xt) ys      = x : merge xt ys
        multiplicative p = [ n * p | n <- [p, p + 2 ..]]


isPandigital n = S.fromList ds == S.fromList [1..(length ds)]
  where ds = map digitToInt $ show n
    

good   = filter isPandigital  $ takeWhile (< 987654321) primes

answer = maximum good

main = print answer
