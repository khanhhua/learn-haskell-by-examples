module Main where

import Control.Monad.State
import Text.Printf
import Data.List

type Name = String
type Age = Int
type Entry = ( Name, Age )


guestBook :: StateT [Entry] IO ()
guestBook = do
    input <- liftIO uiMenu
    case input of 
        '1' -> do
            entries <- get
            liftIO $ uiSearch entries
            guestBook
        '2' -> do
            entry <- liftIO uiEnterNewEntry
            entries <- get
            put (entry : entries)
            guestBook
        'q' -> do
            return ()
        _ ->
            guestBook
    


uiMenu :: IO Char
uiMenu = do
    putStrLn "\n== GUESTBOOK =="
    putStrLn "1. Search"
    putStrLn "2. Enter new entry"
    putStrLn "q. Quit\n"
    getChar


uiSearch :: [Entry] -> IO ()
uiSearch entries = do
    putStrLn "\n== SEARCH =="
    putStr "Keyword: "
    keyword <- getLine
    let filtered = filter (\(name, _) -> keyword `isPrefixOf` name) entries
        list = intercalate "\n" $ map (\(name, age) -> printf "%s: %d" name age) filtered
    putStrLn $ printf "Found: %d" (length filtered)
    putStrLn list


uiEnterNewEntry :: IO Entry
uiEnterNewEntry = do
    putStrLn "\n== NEW ENTRY =="
    putStr "Name: "
    name <- getLine
    putStr "Age: "
    ageS <- getLine

    return ( name, read ageS :: Int )
    

main :: IO ()
main = 
    let initialState = []
    in runStateT guestBook initialState >> return ()
