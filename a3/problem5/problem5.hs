-- Name : Huang Jiaming
-- NSID : jih211
-- StuID: 11207964

import Data.Numbers.Primes
-- my solution, can only find first four in seconds..
perfectNums :: [Integer]
perfectNums = [x | x<-[1..],(sum [y | y<-[1..x`div`2],x`mod`y==0])==x]

firstFour :: [Integer]
firstFour = take 4 perfectNums


-- from wikipedia ,generates the list with expression 2^(p-1) * (2^p-1),where
-- 2^p -1 is a Mersenne prime.All even perfect numbers are of this form. It is
-- not known whether there are any odd perfect numbers.[1] As of 2016 there are
-- 49 known perfect numbers in total

firstEight :: [Integer]
firstEight = take 8 [2^(p-1)*(2^p-1) | p<-primes, isPrime(2^p-1)]
