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

consecutive' ps@(p:pt) ns n
  | n < p = []
  | n == s = ns
  | n > s = consecutive' pt (p:ns)    n
  | n < s = consecutive' ps (init ns) n
              where s = sum ns

consecutiveSum = consecutive' primes []

sums = map (\x -> (x, length . consecutiveSum $ x))
       $ takeWhile (< 1000000) primes

answer = fst $ maximumBy (comparing snd) sums


-- Takes a lot of time
main   = print answer
