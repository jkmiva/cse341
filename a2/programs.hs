-- Name:  Huang Jiaming
-- NSID:  jih211
-- StuID: 11207964

-- Program 2
curry :: ((t1, t2) -> t) -> t1 -> t2 -> t
curry fn a b = fn (a, b)

uncurry :: (t1 -> t2 -> t) -> (t1, t2) -> t
uncurry fn (a, b) = fn a b

-- Program 3
data MyFloat = MyFloat Integer Integer deriving (Show,Eq)

instance Ord MyFloat where
  (<=) f1 f2
    |f1==f2 =True
    |(whole f1)/=(whole f2) =(whole f1)<(whole f2)
    |otherwise =(fraction f1)<(fraction f2)

instance Num MyFloat where
  negate (MyFloat a b) = MyFloat (-a) b
  abs (MyFloat a b) = MyFloat (abs(a)) b
  (+) (MyFloat a1 b1) (MyFloat a2 b2)
    |b1==b2 =MyFloat (a1+a2) b1
    |b1<b2  =MyFloat c b2
    |otherwise = (MyFloat a2 b2) + (MyFloat a1 b1)
    where c = a2*(10^(fracBits(MyFloat a1 b1)-fracBits(MyFloat a2 b2))) + a1
  (-) a b = a + (-b)
  (*) (MyFloat a1 b1) (MyFloat a2 b2) = MyFloat c d
    where c = a1*a2
          d = b1+b2- (integerBits a1 + integerBits a2) + integerBits c
  signum a = error "not implemented"
  fromInteger a = error "not implemented"

instance Fractional MyFloat where
  (/) (MyFloat a1 b1) (MyFloat a2 b2)
    |a2==0 =error "divider can not be zero"
    |otherwise  =MyFloat t1 (b1 - b2 - diff - 6 + integerBits t1)
    where t1 = round (toRational((fromIntegral a1/ fromIntegral a2)*(10^6)))    -- precision >= 10^5
          diff = integerBits a1 - integerBits a2
  fromRational a = error "not implemented"

integerBits :: Integer -> Integer
integerBits n = toInteger $ length $ numList n

fracBits :: MyFloat -> Integer
fracBits (MyFloat a b) = lenA - b
  where lenA = toInteger $ length $ numList a

-- for test
a = MyFloat 3297 2
b = MyFloat 329 5
c = MyFloat (-32000) 5
d = MyFloat (-3297) 2
e = MyFloat 4398 3

-- return a list representation of an integer
numList :: Integral a => a -> [a]
numList n = helper (abs(n)) []
  where helper n lst
          |n==0 =lst
          |otherwise = helper (n `div` 10) ((n `mod` 10):lst)

-- internal function
_whole :: MyFloat -> Integer
_whole (MyFloat a b) = partNums b lst 0 False
  where lst = numList a
        partNums bb (x:xs) ret last
          |bb==0                     =ret
          |(bb/=0 && length(xs)==0 && last==False)  =partNums (bb-1) (x:xs) (ret*10+x) True
          |(bb/=0 && length(xs)==0 && last==True)   =partNums (bb-1) (x:xs) (ret*10) last
          |otherwise                 =partNums (bb-1) xs (ret*10+x) last

whole :: MyFloat -> Integer
whole (MyFloat a b)
  |a==0       = 0
  |a<0       = -(_whole (MyFloat a b))
  |otherwise = _whole (MyFloat a b)

fraction :: MyFloat -> Double
fraction (MyFloat a b)
  |a==0              =0
  |(toInteger(length fullLst) <= b) =0
  |otherwise         =fromIntegral(partNums (toInteger(length fullLst) - b) lst 0) / fromIntegral(10^(toInteger(length fullLst) -b))
  where fullLst = numList a
        lst = drop (fromIntegral b) fullLst
        partNums bb (x:xs) ret
          |length(xs)==0 =ret*10+x
          |otherwise =partNums (bb-1) xs (ret*10+x)

-- problem 4
shuffle :: [a] -> [a] -> [a]
shuffle [] xs2 = xs2
shuffle xs1 [] = xs1
shuffle (x1:xs1) xs2 = x1:(shuffle xs2 xs1)

-- problem 5
split :: [a] -> Integer -> ([a],[a])
split xs 0 = ([],xs)
split [] _ = ([],[])
split (x:xs) n = (x:xs1, xs2)
  where (xs1,xs2) = split xs (n-1)

--problem 6
nshuffle :: Integer -> Integer -> [Char]
nshuffle c n = helper bLst rLst n
  where bLst = replicate (fromIntegral(c)) 'b'
        rLst = replicate (fromIntegral(c)) 'r'
        helper l1 l2 n1
          |n1==0     =l1++l2
          |otherwise =(helper (fst (split (shuffle l1 l2) c)) (snd (split (shuffle l1 l2) c)) (n1-1))

--problem 7
--TODO ugly version
consecutive :: Eq a =>[a] -> Integer
consecutive [] =0
consecutive xs = _consecutive xs 1 1
  where _consecutive [] _ largest = largest
        _consecutive (x:[]) _ largest= largest
        _consecutive (x1:x2:xs) temp largest
          |x1==x2 =_consecutive (x2:xs) (temp+1) (max (temp+1) largest)
          |otherwise =_consecutive (x2:xs) 1 largest
