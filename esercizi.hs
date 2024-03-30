
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)


comb n k = div (factorial n) (factorial (n-k) * factorial k)

allcomb n = map (comb n) [0..n]

removeEvenIndexes :: [a] -> [a]
removeEvenIndexes xs = [x | (x, index) <- zip xs [1..], odd index]

sumOddIndexes :: Num a => [a] -> a
sumOddIndexes xs = sum (removeEvenIndexes xs)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x] 
        biggerSorted = quicksort [a | a <- xs, a > x]   
    in  smallerSorted ++ [x] ++ biggerSorted  


removeEvenNumbers :: (Integral a) => [a] -> [a]
removeEvenNumbers xs = [x | x <- xs, odd x]

minOdd :: (Integral a) => [a] -> [a]
minOdd [] = []
minOdd (x:xs)
    | length (removeEvenNumbers xs) >=2  = 
        let ordedOdds = quicksort (removeEvenNumbers xs)
        in take 2 ordedOdds
    | otherwise = []

coupleList :: (Integral a) => [a] -> [(a, a)]
coupleList [] = []    
coupleList (x:xs) = (x, sum xs) : coupleList xs

coupleListReversed :: (Integral a) => [a] -> [(a, a)]
coupleListReversed [] = []    
coupleListReversed (x:xs) = finalList xs 0
    where 
        finalList [] _ = []
        finalList (x:xs) total = (x,total) : finalList xs (total + x)


shiftToZero :: (Num a, Ord a) => [a] -> [a]
shiftToZero xs = shift xs (minimum xs)
  where
    shift [] _ = []
    shift (y:ys) minTail = let shiftedY = y - minTail
                            in shiftedY : shift ys (min minTail y)




