-- Name : Huang Jiaming
-- NSID : jih211
-- StuID: 11207964

altMap :: (t -> a) -> (t -> a) -> [t] -> [a]
altMap g h [] = []
altMap g h (x:xs) = (g x) : altMap h g xs

luhnDouble :: Int -> Int
luhnDouble x | 2*x <= 9   = 2*x
             | otherwise  = 2*x - 9

--- solution here
luhn :: [Int] -> Bool
luhn xs = ret==0
  where ret = (sum (altMap id luhnDouble xs')) `mod` 10
        xs' = reverse xs
