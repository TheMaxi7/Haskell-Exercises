matrixDim :: [[a]] -> (Int, Int)
matrixDim matrix = if isMatrix matrix then (numRows, numCols) else (-1,-1)
  where
    numRows = length matrix
    numCols = if null matrix then 0 else length (head matrix)

isMatrix :: [[a]] -> Bool
isMatrix [] = True 
isMatrix matrix = allSameLength (map length matrix)
  where
    allSameLength :: [Int] -> Bool
    allSameLength [] = True
    allSameLength [_] = True
    allSameLength (x:y:xs)
      | x == y = allSameLength (y:xs)
      | otherwise = False

colSums :: [[Int]] -> [Int]
colSums [] = []
colSums matrix = map sumColumn [0 .. (length (head matrix) - 1)]
  where
    sumColumn col = sum [row !! col | row <- matrix]

colAltSums :: [[Int]] -> [Int]
colAltSums [] = []
colAltSums matrix = map sumAltColumn [0 .. (length (head matrix) - 1)] 
    where 
        sumAltColumn col = sum [if odd col then row !! col else -(row !! col) |row <-matrix]


colMinMax :: [[Int]] -> [(Int, Int)]
colMinMax [] = []   
colMinMax matrix = map minMax [0 .. (length (head matrix) - 1)]
    where 
        minMax columnIdx = 
            let column = map (!! columnIdx) matrix
                min = minimum column
                max = maximum column
            in (min,max)

lowerTriangular :: [[Int]] -> Bool
lowerTriangular [] = True
lowerTriangular matrix = all checkZeros [(rowId, colId) | colId <- [0 .. length (head matrix) - 1], rowId <- [0 .. length matrix - 1], rowId < colId]
  where
    checkZeros (rowId, colId)
       | matrix !! rowId !! colId == 0  = True
       | otherwise = False


upperTriangular :: [[Int]] -> Bool
upperTriangular [] = True
upperTriangular matrix = all checkZeros [(rowId, colId) | colId <- [0 .. length (head matrix) - 1], rowId <- [0 .. length matrix - 1], rowId > colId]
  where
    checkZeros (rowId, colId)
       | matrix !! rowId !! colId == 0  = True
       | otherwise = False