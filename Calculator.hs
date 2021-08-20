module Calculator where

import Control.Monad.State

data Button
    = Clear
    | Equal
    | Digit Char
type CalculatorResult = Float
type LcdFigure = String
type CalculatorState = (CalculatorResult, LcdFigure)


makeCalculator :: CalculatorState
makeCalculator = (0.0, "0")


press :: Button -> State CalculatorState ()
press Clear = put (0, "0")
press Equal = do
    (result, lcd) <- get
    if result == 0.0
    then 
        let updatedResult = read lcd :: Float
        in put (updatedResult, show updatedResult)
    else put (result, show result)
press (Digit char) = do
    ( result, lcd) <- get
    if lcd == "0"
    then put (result, [char])
    else put (result, lcd ++ [char])


calcSequence ::State CalculatorState LcdFigure
calcSequence = do
    press Clear
    press (Digit '1')
    press (Digit '2')
    press (Digit '3')
    press Equal

    (_, lcd) <- get
    return lcd

main :: IO ()
main = print $ evalState calcSequence makeCalculator
