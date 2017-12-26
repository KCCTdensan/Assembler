module Utils.Numeric (
	toBin
) where

import Numeric
import Data.Char

showBin :: String -> String
showBin x = showIntAtBase 2 intToDigit (read x) ""

padding :: Int -> String -> Either String String
padding width x =
	let diff = width - length x
	in if width < length x
		then Left $ "錯誤 (表現可能な範囲を超過): 幅" ++ show width ++ "迄之処 >> " ++ x
		else Right $ ['0' | _ <- [1..diff]] ++ x

toBin :: Int -> String -> Either String String
toBin n x = padding n $ showBin x
