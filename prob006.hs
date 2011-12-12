square x = x * x

sumOfSquares = sum . map square

squareOfSum  = square . sum

difference r = (squareOfSum r) - (sumOfSquares r)

answer       = difference [1..100]

main         = do print answer
