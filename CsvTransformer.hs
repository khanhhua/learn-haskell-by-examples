import System.IO( isEOF )
import Text.Printf
import Data.Char (isSpace)
import Data.List
import Data.Map (Map, fromList, toList)

data Line
    = Header [String]
    | Row [String]

data JsonValue
    = Str String
    | Int Integer
    | Flt Float
    | Bln Bool
    | Arr [JsonValue]
    | Obj (Map String JsonValue)

main :: IO ()
main = do
    let headers = ["Id", "Name", "Dob"]
    processLine (writeToFile . lineToJson )headers [] 


encode :: JsonValue -> String
encode (Str value_) = printf "\"%s\"" value_
encode (Int value_) = printf "%d" value_
encode (Flt value_) = printf "%f" value_
encode (Bln value_) = if value_ then "true" else "false"
encode (Arr values_) = "[" ++ ((intercalate ", ") (map encode values_)) ++ "]"
encode (Obj values_) = let encoded = "{" ++ ((intercalate ", ")
                            ( map (\(key, value) ->
                                printf "\"%s\": %s" key (encode value)
                            ) (toList values_))) ++ "}"
                        in printf "%s" encoded


processLine :: ([Line] -> IO ()) -> [String] -> [Line] -> IO ()
processLine transformer colNames [] = do
    rawLine <- getLine
    processLine transformer colNames ((Header colNames):[])
processLine transformer colNames lines = do
    eof <- isEOF
    if eof then
        transformer (reverse lines)
    else do
        rawLine <- getLine
        processLine transformer colNames ((Row (split rawLine)):lines)


lineToJson :: [Line] -> String
lineToJson [] = ""
lineToJson ((Header columnNames):lines) =
    encode $ Arr $ foldl (\acc line ->
        case line of
            Header _ -> acc
            Row values ->
                let dict = fromList $ map (\(name, value) ->
                            ( name, Str value )
                        ) (zip columnNames values)
                in (Obj dict):acc
        ) [] lines

writeToFile :: String -> IO ()
writeToFile json = do
    putStrLn "Writing to out.json..."
    writeFile "out.json" json


split :: String -> [String]
split s =  case dropWhile isSpace s of
    "" -> []
    s' -> w : split s''
        where (w, s'') = break isSpace s'
