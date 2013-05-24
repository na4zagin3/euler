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

pythagoreanTriples :: [(Integer,Integer,Integer)]
pythagoreanTriples = [ (a, b, c) | c <- [1..], a <- [1..(c-1)], b <- return . isqrt $ (c^2 - a^2), a <= b, a^2 + b^2 == c^2]

main :: IO ()
main = print $ (\ (a, b, c) -> a * b * c) . head . filter (\ (a, b, c) -> a + b + c == 1000) $ pythagoreanTriples
