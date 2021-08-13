import Text.Printf

main :: IO ()
main = do
    putStrLn "Welcome to Calc"
    loop

toInt :: String -> IO Integer
toInt s = return (read s::Integer)

loop = do
    putStr "Enter A: "
    a <- getLine >>= toInt
    putStr "Enter B: "
    b <- getLine >>= toInt

    putStr (printf "Result: %d + %d = %d\n" a b (a + b))

    loop
