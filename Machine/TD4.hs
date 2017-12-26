module Machine.TD4 (
	assemble
	-- disassemble
) where

import Text.Regex

import qualified Utils.Numeric as Util

width :: Int
width = 4

toBin :: String -> Either String String
toBin = Util.toBin width

-- [要修正]: 錯誤の在る行番號の表示に對應す可し
assemble :: [String] -> [Either String String]
assemble insts = map toCode insts

toCode :: String -> Either String String
toCode inst =
	let	wordlist = splitRegex (mkRegex " +") inst
		opcode = head wordlist
		operand = tail wordlist -- [要修正]: 第一項の末尾に,が附されてゐる場合を排除。氣持惡き故。
	in case opcode of
		"mov"	-> mov operand
		"add"	-> add operand
		"in"		-> Right $ in_op operand
		"out"	-> out operand
		"jmp"	-> jmp operand
		"jnc"	-> jnc operand
		_		-> Left $ "錯誤 (存在爲無い命令): " ++ inst

mov :: [String] -> Either String String
mov operand =
	case operand !! 0 of
		"a," ->
			case operand !! 1 of
				"b"	-> Right "00010000"
				im	-> do
					binIm <- toBin im
					Right $ "0011" ++ binIm
		"b," ->
			case operand !! 1 of
				"a"	-> Right "01000000"
				im	-> do
					binIm <- toBin im
					Right $ "0111" ++ binIm

add :: [String] -> Either String String
add operand =
	case operand !! 0 of
		"a," -> do
			binIm <- toBin $ operand !! 1
			Right $ "0000" ++ binIm
		"b," -> do
			binIm <- toBin $ operand !! 1
			Right $ "0101" ++ binIm

in_op :: [String] -> String
in_op operand =
	case operand !! 0 of
		"a" -> "00100000"
		"b" -> "01100000"

out :: [String] -> Either String String
out operand =
	case operand !! 0 of
		"b"	-> Right "10010000"
		im	-> do
			binIm <- toBin im
			Right $ "1011" ++ binIm

jmp :: [String] -> Either String String
jmp operand = do
	binIm <- toBin $ operand !! 0
	Right $ "1111" ++ binIm

jnc :: [String] -> Either String String
jnc operand = do
	binIm <-toBin $ operand !! 0
	Right $ "1110" ++ binIm
