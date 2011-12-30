
fibs = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)
evenFibs = filter even fibs
result = sum $ takeWhile (< 4000000) evenFibs
main =  print result
