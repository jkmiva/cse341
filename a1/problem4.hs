-- Author : Huang Jiaming
-- StuID  : 11207964
-- NSID   : jih211

-- a)
averageThree :: Int -> Int -> Int -> Double
averageThree a b c = (fromIntegral (a+b+c))/3

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c = length (filter (> avg) lst)
  where avg = averageThree a b c
        lst = [fromIntegral a, fromIntegral b, fromIntegral c]

-- b)
averageThreeInOne :: (Int, Int, Int) -> Double
averageThreeInOne (a, b, c) = averageThree a b c


-- c)
larger :: [Double] -> Double
larger lst
  | head lst > head (tail lst) = head lst
  | otherwise                  = head (tail lst)

smaller :: [Double] -> Double
smaller lst
  | head lst < head (tail lst) = head lst
  | otherwise                  = head (tail lst)

maxN :: Int -> Int -> Int -> Double
maxN a b c
  | howManyAboveAverage a b c == 1 = head (filter (> avg) lst)
  | otherwise                      = larger (filter (> avg) lst)
  where avg = averageThree a b c
        lst = [fromIntegral a, fromIntegral b, fromIntegral c]

minN :: Int -> Int -> Int -> Double
minN a b c
  | howManyAboveAverage a b c == 1 = smaller (filter (<= avg) lst)
  | otherwise                      = head (filter (< avg) lst)
  where avg = averageThree a b c
        lst = [fromIntegral a, fromIntegral b, fromIntegral c]

midN :: Int -> Int -> Int -> Double
midN a b c
  | howManyAboveAverage a b c == 1 = larger (filter (<= avg) lst)
  | otherwise                      = smaller (filter (> avg) lst)
  where avg = averageThree a b c
        lst = [fromIntegral a, fromIntegral b, fromIntegral c]

orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a, b, c)
  | howManyAboveAverage a b c == 0 = (a, b, c)
  | otherwise                      = (round (minN a b c), round (midN a b c), round (maxN a b c))
