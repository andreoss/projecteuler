factorial n   = product [1..n]
binomial  n k = factorial n / factorial k * factorial (n - k)
latice    n   = binomial (n * 2) n

answer = round $ latice 20
main   = print answer
