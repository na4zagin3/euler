x = 1000
main = do
        putStrLn . show . sum $ filter (\n -> any (\a -> mod n a == 0)  [3, 5]) [1..(x-1)]
