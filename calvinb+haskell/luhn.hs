import Data.Char
import System.IO

minLen = 14
maxLen = 16

finalChar :: (Char, Bool) -> Char
finalChar (c, x) = if x then 'X' else c

doubleDigitSum :: Int -> Int
doubleDigitSum i =
    let twice = i * 2
    in twice `div` 10 + twice `mod` 10

luhn :: String -> String
luhn cs =
    let luhn' :: [(Char, Bool)] -> [(Char, Bool)] -> Int -> Int -> String -> String
        luhn' [] bs _ _ result = (map finalChar bs) ++ result
        luhn' cs bs digitCount _ result
            | digitCount == maxLen =
                let (b:bs') = reverse bs
                    result' = ((finalChar b):result)
                    cs' = bs' ++ cs
                in  luhn' cs' [] 0 0 result'
        luhn' ((c, x):cs) bs digitCount digitSum result
            | isDigit c =
                let digitCount' = digitCount + 1
                    digitSummer = if digitCount' `mod` 2 == 0
                                    then doubleDigitSum
                                    else id
                    digitSum' = digitSum + digitSummer (read [c])
                in  if digitCount' < minLen
                    then luhn' cs ((c, x):bs) digitCount' digitSum' result
                    else
                        let xer = if digitSum' `mod` 10 == 0
                                    then map (\(c, x) -> (c, isDigit c))
                                    else id
                            bs' = xer ((c, x):bs)
                        in luhn' cs bs' digitCount' digitSum' result
            | otherwise = luhn' cs ((c, x):bs) digitCount digitSum result

    in luhn' (reverse $ map (\c -> (c, False)) cs) [] 0 0 ""

main = do
    input <- getLine
    if null input
    then return ()
    else do
        putStrLn $ luhn input
        hFlush stdout
        main
