import Data.Maybe
import Data.List
limit = 1500000
pythagoras (a, b, c) = a^2 + b^2 == c^2


euclid = [ [a,b,c] |
           m<-[2..limit],
           n<-[1..m],
           k<-[1..3],
           m > n,
           odd (m - n),
           gcd m n == 1,
           let a = k * (m^2 - n^2)
               b = k * 2*m*n
               c = k * (m^2+n^2), a+b+c <= limit]

perims = map (\[a,b,c] -> a+b+c) euclid
