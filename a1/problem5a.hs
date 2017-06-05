-- Author : Huang Jiaming
-- StuID  : 11207964
-- NSID   : jih211

f :: Double -> Double
f x = x + 1
g :: Double -> Double
g x = x * 2
h :: Double -> Double
h x = x / 3

compose3 :: (Double -> Double) -> (Double -> Double) -> (Double -> Double) -> Double -> Double
compose3 f g h x = f(g (h x))

