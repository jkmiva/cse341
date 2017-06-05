-- Name : Huang Jiaming
-- NSID : jih211
-- StuID: 11207964

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
  |p x =[]
  |otherwise =(h x):unfold p h t (t x)

-- (a)
map :: (t -> a) -> [t] -> [a]
map f = helper f
  where helper f xs = unfold p h t xs
          where h = f.head
                t = tail
                p xs = case xs of (x:xs) -> False
                                  []     -> True
-- (b)
iterate :: (t -> t) -> t -> [t]
iterate f = helper f
  where helper f x = x : unfold (\_ -> False) f f x
