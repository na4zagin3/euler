x :: Integer
x = 600851475143

factors :: Integer -> [Integer]
factors k = gen k 2
    where
        gen n d
            | n `mod` d == 0 = d : gen (n `div` d) d
            | n >= d       = gen n (d + 1)
            | otherwise    = []

main :: IO ()
main = print . last $ factors x
