import qualified Data.Set as Set

triangle 	n = n * (n + 1)     `div` 2 	  	
pentagonal 	n = n * (3 * n - 1) `div` 2 	  	
hexagonal 	n = n * (2 * n - 1) 

range             = [1..1000000]

triangles         = map triangle   range
pentagonals       = map pentagonal range
hexagonals        = map hexagonal  range

answer            = [ n | n <- triangles, isPentagonal n, isHexagonal n] !! 2
                    where isHexagonal  = flip Set.member $ Set.fromList hexagonals
                          isPentagonal = flip Set.member $ Set.fromList pentagonals


main   = print answer
