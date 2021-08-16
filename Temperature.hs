import Text.Printf
import Data.Char (isSpace)
import Data.List (unfoldr)

toUnit :: String -> IO Unit
toUnit input = do
    let t = init input
        unit = last input
        value = read t::Float
    return (
        case unit of
            'F' -> Fahrenheit value
            'C' -> Celcius value
        )


data Unit
    = Celcius Float
    | Fahrenheit Float

instance Show (Unit) where
    show (Celcius t) = printf "%fC" t
    show (Fahrenheit t) = printf "%fF" t

convert :: Unit -> Unit
convert temp =
    case temp of
        Celcius t -> Fahrenheit ((t * 9.0/5.0) + 32)
        Fahrenheit t -> Celcius ((t - 32) / 5.0 * 9.0)


main :: IO ()
main = do
    putStrLn "=== Temperature ==="
    putStr "How hot is it? "

    temp <- getLine >>= toUnit
    let converted = convert temp
    putStrLn (printf "Converted: %s" (show converted))
