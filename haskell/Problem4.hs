import Data.List

x :: Integer
x = 600851475143

factors :: Integer -> [Integer]
factors k = gen k 2
    where
        gen n d
            | n `mod` d == 0 = d : gen (n `div` d) d
            | n >= d       = gen n (d + 1)
            | otherwise    = []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

oneUnit :: (Enum a, Eq a, Num a) => a -> a -> [a]
oneUnit k n = [ if i == n then 1 else 0 | i <- [1..k] ]

lowerstairDoubles :: (Num a, Enum a) => a -> [(a, a)]
lowerstairDoubles n = [ (x, y) | x <- reverse [1..n], y <- reverse [x..n] ]

doubleSeq :: (Num a, Enum a, Ord a) => a -> [a]
doubleSeq n = reverse $ sort $ map (\ (x, y) -> x * y) $ lowerstairDoubles n

main :: IO ()
main = print . head $ filter (isPalindrome . show) $ doubleSeq 999
