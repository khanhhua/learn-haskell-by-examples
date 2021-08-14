import System.Exit
import Text.Printf
import Data.List

main :: IO ()
main = do
    putStrLn "=== PhoneBook ==="
    loop

loop :: IO ()
loop = do
    choice <- menu
    putStrLn (printf "\nChoice: %c" choice)
    case choice of
        'q' -> return ()
        'e' ->
            uiEnterPhoneNumber >> loop
        's' -> 
            uiSearchContacts >> loop
        _ -> loop

menu :: IO Char
menu = do
    let items = intercalate "\n" [ "E.nter new phone", "S.earch contacts", "Q.uit"]
    putStrLn items
    getChar

uiEnterPhoneNumber :: IO ()
uiEnterPhoneNumber = do
    putStrLn "\n=== Enter Phone Number ==="
    putStr "Name: "
    name <- getLine
    putStr "Number: "
    number <- getLine
    putStrLn (printf "Saving new phone %s under %s..." number name)

    return ()

uiSearchContacts :: IO ()
uiSearchContacts = do
    putStrLn "\n=== Search Contacts ==="
    putStr "Keyword: "
    keyword <- getLine
    putStrLn "Searching contacts..."

    putStrLn "S.earch again | B.ack"
    choice <- getChar
    case choice of
        's' -> uiSearchContacts
        'b' -> return ()
        _ -> uiSearchContacts
