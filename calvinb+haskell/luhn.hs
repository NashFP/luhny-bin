-- Didn't bother with the whole file thing yet.
-- Here are just the functions for the Luhn check and the credit card format.

import Data.Char

isLuhn :: [Char] -> Bool
isLuhn digits = 
  let 
    digitSum x  | x < 10 = x
                | otherwise = x `mod` 10 + digitSum (x `div` 10)
    withDoubles = reverse $ zipWith ($) (cycle [id, (*2)]) (reverse $ map digitToInt digits)
    digitSumTotal = sum $ map digitSum withDoubles
  in digitSumTotal `mod` 10 == 0

isCreditCard :: [Char] -> Bool
isCreditCard s =
  let
    legalChar x = isDigit x || x `elem` "- "
    digits = filter isDigit s
  in all legalChar s && length digits `elem` [14..16] && isLuhn digits
