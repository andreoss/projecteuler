answer = product $ head [ [a, b, c]
          | a <- [1 .. 1000],
            b <- [1 .. a],
            let c = 1000 - a - b,
            a ^ 2 + b ^ 2 == c ^ 2 ]
main   = print answer
