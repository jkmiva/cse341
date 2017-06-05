-- Author : Huang Jiaming
-- StuID  : 11207964
-- NSID   : jih211

-- a)
fastExp1 :: Double -> Int -> Double
fastExp1 n k = if (k == 0) then 1
               else if (even k) then (fastExp1 n (k `div` 2))^^2
               else (n * fastExp1 n (k-1))

-- b)
fastExp2 :: Double -> Int -> Double
fastExp2 n k
  | k == 0    = 1
  | even k    = (fastExp2 n (k `div` 2))^^2
  | otherwise = n * fastExp2 n (k-1)

-- c)
fastExp3 :: Double -> Int -> Double
fastExp3 n 0 = 1
fastExp3 n k
  | even k    = (fastExp3 n (k `div` 2))^^2
  | otherwise = n * fastExp3 n (k-1)
