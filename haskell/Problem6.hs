x :: Integer
x = 100

sumOfSquaresFromOne :: Integer -> Integer
sumOfSquaresFromOne n = (`quot` 6 ) $ n * (n + 1) * (2 * n + 1)

sumOfSequenceFromOne :: Integer -> Integer
sumOfSequenceFromOne n = (`quot` 2 ) $ n * (n + 1)


main :: IO ()
main = print $ (sumOfSequenceFromOne x)^(2::Integer) - sumOfSquaresFromOne x
