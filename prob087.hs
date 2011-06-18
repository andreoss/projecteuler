import qualified Data.Set as S
import Data.Ord  (comparing)
import Data.List (maximumBy)

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

limit        = 50 * 10^6
primesSquare = takeWhile (< limit) $ map (^ 2) primes
primesCube   = takeWhile (< limit) $ map (^ 3) primes
primesForth  = takeWhile (< limit) $ map (^ 4) primes

sums = [ a + b + c
       | a <- primesSquare, b <- primesCube, c <- primesForth,
         a + b + c < limit ]

answer = S.size $ S.fromList sums
main   = print answer
