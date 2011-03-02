import Data.List
import Data.List (maximumBy)
import Data.Ord  (comparing)
-- 
merge :: (Ord a) => [a] -> [a] -> [a]
merge []        ys        = ys
merge xs        []        = xs
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)


diff :: (Ord a) => [a] -> [a] -> [a]
diff []         ys       =  ys
diff xs         []       =  xs
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt

primes, nonprimes :: [Integer]
primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes) 
nonprimes = foldr1 f $ map multiplicative $ tail primes
  where f (x:xt) ys      = x : (merge xt ys)
        multiplicative p = [ n * p | n <- [p, p + 2 ..]]

-- Problem 27

range = [-1000..1000]
-- formula ::  -> Integer -> Integer
formula a b n = n^2 + a * n + b

isPrime 1 = False
isPrime n
  | n <= 0     = False
  | otherwise =  n' == n
  where n' = last $ takeWhile (<= n) primes

-- optimization, b is prime 
bCoefficients = filter isPrime range

answer = product
         $ fst
         $ maximumBy (comparing snd)
         [ ( [a, b], numberOfPrimeProduced )  |
           a <- range,
           b <- bCoefficients,
           let numberOfPrimeProduced =
                 length
                 $ takeWhile isPrime
                 $ map (formula a b) [1..] ]


main = do print answer 
         
