-- Author : Huang Jiaming
-- StuID  : 11207964
-- NSID   : jih211

-- a)
luhnDouble :: Int -> Int
luhnDouble n
  | (n * 2 > 9) = n * 2 - 9
  | otherwise   = n * 2

-- b)
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d
  | ret `mod` 10 == 0 = True
  | otherwise         = False
  where ret = luhnDouble a + b + luhnDouble c + d
