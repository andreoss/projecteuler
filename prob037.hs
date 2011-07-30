import qualified Data.Set as S

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt

primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes) 
nonprimes = foldr1 f $ map multiplicative $ tail primes
  where f (x:xt) ys      = x : (merge xt ys)
        multiplicative p = [ n * p | n <- [p, p + 2 ..]]

primesS   = S.fromList $ takeWhile (< 1000000) primes
isPrime n = S.member n primesS

-- rotations :: Integer -> [Integer]
-- rotations n = [ r
--   | i <- [1..(length ss)], let r = read ((drop i ss)++(take i ss)) :: Integer ]
--               where ss = show n

truncations n  = [ r | i <- [1..length ss], let r = read $ take i ss :: Integer] ++
                 [ r | i <- [1..length ss - 1], let r = read $ drop i ss :: Integer]
  where ss = show n

isIntresting n = and $ map isPrime $  truncations n

ofIterest = S.filter isIntresting primesS
answer    = S.foldr (+) 0 $ ofIterest `S.difference` S.fromList [2,3,5,7]
main      = do print answer


