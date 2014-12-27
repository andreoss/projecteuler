colors = [1..7]

expected step = sum [
  1 - product [ 1 - (step/(n_r - i)) | i <- [0..((n_b ) - 1)] ]
  |
  k <- [0..(length n_d)]
               ] where n_d = length colors
                       n_r = 70
                       n_b = n_r / n_d
