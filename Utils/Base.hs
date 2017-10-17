module Utils.Numeric (
	toBin
) where

import Numeric
import Data.Char

showBin :: String -> String
showBin x = showIntAtBase 2 intToDigit (read x) ""

padding :: Int -> String -> String
padding n x =
	let diff = n - length x
	in if n < length x
		then error $ "錯誤 (表現可能な範囲を超過): " ++ x
		else ['0' | x <- [1..diff]] ++ x

toBin :: Int -> String -> String
toBin n x = padding n $ showBin x
