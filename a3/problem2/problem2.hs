-- Name : Huang Jiaming
-- NSID : jih211
-- StuID: 11207964

altMap :: (t -> a) -> (t -> a) -> [t] -> [a]
altMap g h [] = []
altMap g h (x:xs) = (g x) : altMap h g xs
