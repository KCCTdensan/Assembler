module Machine.TD4 (
	translate
) where

import Text.Regex

-- [要修正]
assemble :: String -> String
assemble op =
	let	wordlist = splitRegex (mkRegex " +") op
		opcode = head wordlist
		operand = tail wordlist
		optype = length operand
	in case opcode of
		"mov"	-> case 
			assemble "mov a, im" = "0011im"
			assemble "mov b, im" = "0111im"
			assemble "mov a, b" = "00010000"
			assemble "mov b, a" = "01000000"
		"add"	->
			assemble "add a, im" = "0000im"
			assemble "add b, im" = "0101im"
		"in"		->
			assemble "in a" = "00100000"
			assemble "in b" = "01100000"
		"out"	->
			assemble "out im" = "1011im"
			assemble "out b" = "10010000"
		"jmp"	->
			assemble "jmp im" = "1111im"
		"jnc"	->
			assemble "jnc im" = "1110im"
