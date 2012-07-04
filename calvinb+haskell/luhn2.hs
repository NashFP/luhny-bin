-- This time I tried steal Dave's technique of subtracting from previous sums
-- instead of recalculating them each time. It's about twice as fast as my
-- first one, but still not very fast. Averaging about 158 ms.

import Data.List
import Data.Char
import Debug.Trace
import System.IO

minLength = 14
maxLength = 16

luhn cs = luhn' cs ""

luhn' [] result = result
luhn' cs result =
    let (illegal, legal, rest) = split cs
        dc = digitCount legal
        legal' = if dc < minLength
                then legal
                else findMax (reverse legal) 0 0 0 1 0 "" ""
    in  luhn' rest $ result ++ illegal ++ legal'

--findMax cs digitCount digitSum otherDigitSum factor shadow crumbs result |
    --trace ("findMax " ++ show (cs, digitCount, digitSum, otherDigitSum, factor, shadow, crumbs, result)) False = undefined
findMax cs digitCount digitSum otherDigitSum factor shadow crumbs result =
    let (nonDigits, rest) = break isDigit cs
    in findMaxSplit nonDigits rest digitCount digitSum otherDigitSum factor shadow crumbs result

findMaxSplit nonDigits [] digitCount digitSum otherDigitSum factor shadow crumbs result
    | digitCount < minLength =
        (reverse nonDigits) ++ (xCrumbs (reverse crumbs) shadow "") ++ result
    | True =
        checkSum "" digitCount digitSum otherDigitSum factor shadow ((reverse nonDigits) ++ crumbs) result

findMaxSplit nonDigits (d:cs) digitCount digitSum otherDigitSum factor shadow crumbs result
    | digitCount == maxLength =
        let crumbs' = (reverse nonDigits) ++ crumbs
        in  checkSum (d:cs) digitCount digitSum otherDigitSum factor shadow crumbs' result
    | True =
        let crumbs' = (d:((reverse nonDigits) ++ crumbs))
            digitCount' = digitCount + 1
            digitSum' = digitSum + (getDigitSum d factor)
            otherFactor = 2 `div` factor
            otherDigitSum' = otherDigitSum + (getDigitSum d otherFactor)
        in  findMax cs digitCount' digitSum' otherDigitSum' otherFactor shadow crumbs' result

--xCrumbs crumbs shadow result |
--    trace ("xCrumbs " ++ show (crumbs, shadow, result)) False = undefined
xCrumbs crumbs 0 result = (reverse crumbs) ++ result
xCrumbs "" _ result = reverse result
xCrumbs (c:crumbs) shadow result
    | isDigit c = xCrumbs crumbs (shadow - 1) ('X':result)
    | True      = xCrumbs crumbs shadow (c:result)

--checkSum cs digitCount digitSum otherDigitSum factor shadow crumbs result |
--    trace ("checkSum " ++ show (cs, digitCount, digitSum, otherDigitSum, factor, shadow, crumbs, result)) False = undefined
checkSum cs digitCount digitSum otherDigitSum factor shadow crumbs result
    | digitSum `mod` 10 == 0 =
        luhnFound cs digitCount digitSum otherDigitSum factor shadow crumbs result
    | True =
        tryShorter cs digitCount digitSum otherDigitSum factor shadow crumbs result
        
luhnFound cs digitCount digitSum otherDigitSum factor shadow crumbs result =
    nextStart cs digitCount digitSum otherDigitSum factor (max digitCount shadow) crumbs result

--tryShorter cs digitCount digitSum otherDigitSum factor shadow crumbs result | 
--    trace ("tryShorter " ++ show (cs, digitCount, digitSum, otherDigitSum, factor, shadow, crumbs, result)) False = undefined
tryShorter cs digitCount digitSum otherDigitSum factor shadow crumbs result
    | digitCount == minLength =
        nextStart cs digitCount digitSum otherDigitSum factor shadow crumbs result
    | True =
        let (nonDigits, (d:crumbs')) = break isDigit crumbs
            cs' = (d:((reverse nonDigits) ++ cs))
            digitCount' = digitCount - 1
            otherFactor = 2 `div` factor
            digitSum' = digitSum - (getDigitSum d otherFactor)
            otherDigitSum' = otherDigitSum - (getDigitSum d factor)
        in  checkSum cs' digitCount' digitSum' otherDigitSum' otherFactor shadow crumbs' result 

--nextStart cs digitCount digitSum otherDigitSum factor shadow crumbs result |
--    trace ("nextStart " ++ show (cs, digitCount, digitSum, otherDigitSum, factor, shadow, crumbs, result)) False = undefined
nextStart cs digitCount digitSum otherDigitSum factor shadow crumbs result =
    let revCrumbs = reverse crumbs
        (nonDigits, (d:revCrumbs')) = break isDigit revCrumbs
        d' = if shadow > 0 then 'X' else d
        result' = (d':((reverse nonDigits) ++ result))
        digitCount' = digitCount - 1
        digitSum' = digitSum - (getDigitSum d 1)
        otherDigitSum' = otherDigitSum - (getDigitSum d 2)
        factor' = 2 `div` factor
        shadow' = max (shadow - 1) 0
        crumbs' = reverse revCrumbs'
    in  findMax cs digitCount' otherDigitSum' digitSum' factor' shadow' crumbs' result'

getDigitSum char factor =
    let numValue = read [char]
        prod = numValue * factor
    in  (prod `div` 10) + (prod `mod` 10)

isLegal c = isDigit c || c `elem` "- "

isIllegal = not . isLegal

split cs =
    let (i,l) = break isLegal cs
        (l',r) = break isIllegal l
    in (i,l',r)

digitCount cs = length $ filter isDigit cs

test2 = "56613959932537\n"
test4 = "6853371389452376\n"
test5 = "49536290423965\n"
test6 = "306903975081421\n"
test7 = "6045055735309820\n"
test11 = "1256613959932537\n"

main = do
        input <- getLine
        if null input
        then return ()
        else do
            putStrLn $ luhn input
            hFlush stdout
            main
