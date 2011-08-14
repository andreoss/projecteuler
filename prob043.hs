import Data.Array

a = array (1,100) [(i,i) | i <- [1..100]]

primes = [1,2,3,5,7,11,13,17]
getIntresting i ds
  | i < 0 = ds


