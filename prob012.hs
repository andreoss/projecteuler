import Data.List

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

primeDivs n = [p | p <- (takeWhile (< n) primes), n `mod` p == 0]

eulerTotient n = round $ (fromIntegral n) * (product $ map (\p -> (p^2 -1)/(p-1)) $ map fromIntegral $ primeDivs n) 


divs n = [ d |  d <- [1..(n-1)], n `mod` d == 0  ]
sumDivs' = sum . divs


factorize' ps n
  | n         == 1 = []
  | n `mod` p == 0 = p : (factorize' ps (n `quot` p))
  | otherwise     = (factorize' (tail ps) n)
    where  p  = head ps

factorize     = factorize' primes

factorizeP    = map (\a -> (head a, length a)) . group . map fromIntegral . factorize


triangular n  = round $ (n / 2) * (n + 1)

numberFactors = product . map (+1) . map snd . factorizeP

triangulars   = map triangular [1..]

answer        = head [ t | t <- triangulars, numberFactors t > 500]

main          = do print answer