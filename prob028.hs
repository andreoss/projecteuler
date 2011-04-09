
spiral x = takeWhile (<= x ^ 2) [ i  | x<-[0..],
           let (a,b) = rounds !! x,
           let inc   = 2 * (x + 1),
           i <- [ a, a+inc .. b-inc ]
              ] where rounds = zip sqs $ tail sqs
                      sqs    = map (^ 2) [1,3 .. ]
  
answer = sum $ spiral 1001
         
main   = do print answer
