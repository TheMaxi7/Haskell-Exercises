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


elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs)
    |n == x    = True
    |otherwise = elem' n xs

nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs)
    |elem' x xs = nub' xs
    |otherwise  = x: nub' xs

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [_] = True
isAsc (x:y:xs)
    |x <= y = isAsc (y:xs)
    |otherwise = False 


hasPath :: [(Int,Int)] -> Int -> Int -> Bool
hasPath [] a b = a == b
hasPath xs a b
    | a == b    = True
    | otherwise =
        let xs' = [(n,m) | (n,m) <- xs, n /= a]
        in or [hasPath xs' m b | (n,m) <- xs, n==a]

add' :: Int -> Int -> Int
add' = (\x -> (\y -> x+y))

doubleList = map (\x -> 2*x)

rev :: [a]->[a]
rev = foldl (\acc x -> x : acc) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x]: (map((:) x) acc)) []