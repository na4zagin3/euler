x :: Integer
x = 4000000

fib :: [Integer]
fib = gen 1 1
    where
      gen i j = i : gen j (i + j)

main :: IO ()
main = do
        putStrLn . show . sum $ filter even $ takeWhile (< x) fib
