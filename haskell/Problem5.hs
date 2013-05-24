x :: Integer
x = 20

main :: IO ()
main = print $ foldl lcm 1 [1..x]
