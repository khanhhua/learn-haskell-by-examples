import Text.Printf

main :: IO ()
main = do
    putStrLn "What is 3x + 1? An always terminating series."
    putStr "What is your N? "
    n <- getLine >>= toInt
    putStrLn ( printf "The series has %d items.\n" (1 + length (series n)) )


series :: Integer -> [Integer]
series n =
    let
        nextTerm = next n
        s = nextTerm : (series nextTerm)
    in
    takeWhile (\t -> t /= 1) s

next :: Integer -> Integer
next n
    | n == 1 = 1
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = n * 3 + 1

toInt :: String -> IO Integer
toInt s = return (read s::Integer)