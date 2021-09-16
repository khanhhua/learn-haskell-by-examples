import Text.Printf

main :: IO ()
main = do
  name <- putStr "What is your name? " >> getLine
  putStrLn (printf "Hi, %s! Wish you a nice day!" name)
