import Control.Monad
import Text.Printf

prompt :: IO String
prompt = putStr "What is your name? " >> getLine

wish :: String -> IO ()
wish name = putStrLn (printf "Hi, %s! Wish you a nice day!" name)

main :: IO ()
main = forever $ prompt >>= wish


