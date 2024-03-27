
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)


comb n k = div (factorial n) (factorial (n-k) * factorial k)

allcomb n = map (comb n) [0..n]