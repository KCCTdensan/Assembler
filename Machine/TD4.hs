module Machine.TD4 (
	assemble
) where

import Text.Regex

import qualified Utils.Numeric as Util

width :: Int
width = 4

toBin :: String -> String
toBin = Util.toBin width

-- [要修正]: 例外処理之追加
assemble :: String -> String
assemble op =
	let	wordlist = splitRegex (mkRegex " +") op
		opcode = head wordlist
		operand = tail wordlist
	in case opcode of
		"mov"	->	mov operand
		"add"	->	add operand
		"in"		->	in_op operand
		"out"	->	out operand
		"jmp"	->	jmp operand
		"jnc"	->	jnc operand
		_		-> error $ "錯誤 (存在為無い命令): " ++ op

mov :: [String] -> String
mov operand =
	case operand !! 0 of
		"a,"	->	case operand !! 1 of
					"b"	-> "00010000"
				 	im	-> "0011" ++ toBin im
		"b,"	->	case operand !! 1 of
					"a"	-> "01000000"
					im	-> "0111" ++ toBin im

add :: [String] -> String
add operand =
	case operand !! 0 of
		"a,"	-> "0000" ++ toBin (operand !! 1)
		"b,"	-> "0101" ++ toBin (operand !! 1)

in_op :: [String] -> String
in_op operand =
	case operand !! 0 of	
		"a"	-> "00100000"
		"b"	-> "01100000"

out :: [String] -> String
out operand =
	case operand !! 0 of
		"b"	-> "10010000"
		im	-> "1011" ++ toBin im

jmp :: [String] -> String
jmp operand = "1111" ++ toBin (operand !! 0)

jnc :: [String] -> String
jnc operand = "1110" ++ toBin (operand !! 0)
