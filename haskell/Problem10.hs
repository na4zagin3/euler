newtonSeq :: (a -> a) -> a -> [a]
newtonSeq f n =
    n : newtonSeq f (f n)

takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
takeWhile2 f xs = result
    where
      ts = takeWhile (uncurry f) $ zip xs (tail xs)
      result | null ts = []
             | otherwise = (fst . head $ ts) : (map snd ts)

isqrt :: (Integral a)=> a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = sqrtSeq n
    where
      next p = quot (p + quot n p) 2
      sqrtSeq m = head . reverse . takeWhile2 (>) $ newtonSeq next m

isPrime :: (Integral a)=> a -> Bool
isPrime n | n < 2       = False
          | otherwise   = all (\m -> mod n m /= 0) $ primeCandidates n

primeCandidates :: (Integral a)=> a -> [a]
primeCandidates n | n <= 2      = []
                  | otherwise   = takeWhile (<= (isqrt n)) primes

primes :: (Integral a)=> [a]
primes = filter isPrime [2..]

main :: IO ()
main = print $ sum . takeWhile (< (2000000 :: Integer)) $ primes
