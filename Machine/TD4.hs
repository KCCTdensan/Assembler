module Machine.TD4 (
	assemble
) where

import Text.Regex

import Utils.Numeric

width :: Int
width = 8

toBin :: String -> String
toBin = .toBin 2

-- [要修正]: 例外処理之追加
assemble :: String -> String
assemble op =
	let	wordlist = splitRegex (mkRegex " +") op
		opcode = head wordlist
		operand = tail wordlist
	in case opcode of
		"mov"	->	case operand !! 0 of
						"a,"	->	case operand !! 1 of
										"b"	-> "00010000"
									 	im	-> "0011" ++ toBin im
						"b,"	->	case operand !! 1 of
										"a"	-> "01000000"
										im	-> "0111" ++ toBin im
		"add"	->	case operand !! 0 of
						"a,"	-> "0000" ++ toBin (operand !! 1)
						"b,"	-> "0101" ++ toBin (operand !! 1)
		"in"		->	case operand !! 0 of
						"a"	-> "00100000"
						"b"	-> "01100000"
		"out"	->	case operand !! 0 of
						"b"	-> "10010000"
						im	-> "1011" ++ toBin im
		"jmp"	-> "1111" ++ toBin (operand !! 0)
		"jnc"	-> "1110" ++ toBin (operand !! 0)
		_		-> error $ "錯誤 (存在為無い命令): " ++ op
