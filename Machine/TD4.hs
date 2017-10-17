module Machine.TD4 (
	assemble
) where

import Text.Regex

import Utils.Base as Base

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
									 	im	-> "0011" ++ dec2bin im
						"b,"	->	case operand !! 1 of
										"a"	-> "01000000"
										im	-> "0111" ++ dec2bin im
		"add"	->	case operand !! 0 of
						"a,"	-> "0000" ++ dec2bin (operand !! 1)
						"b,"	-> "0101" ++ dec2bin (operand !! 1)
		"in"		->	case operand !! 0 of
						"a"	-> "00100000"
						"b"	-> "01100000"
		"out"	->	case operand !! 0 of
						"b"	-> "10010000"
						im	-> "1011" ++ dec2bin im
		"jmp"	-> "1111" ++ dec2bin (operand !! 0)
		"jnc"	-> "1110" ++ dec2bin (operand !! 0)
		_		-> error $ "錯誤 (存在為無い命令): " ++ op
