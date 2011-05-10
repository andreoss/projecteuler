

solve    []     s         = 0

solve    (c:ct) 0         = 1

solve    (c:ct) s | c > s = solve ct s

solve cs@(c:ct) s         = (solve cs (s - c)) + (solve ct s)

answer = solve [200,100,50,20,10,5,2,1] 200
main   = do print answer
