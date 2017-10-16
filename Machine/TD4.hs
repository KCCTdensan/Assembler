module Machine.TD4 (
	translate
) where

-- [要修正]
translate :: String -> String
translate "mov a, im" = "0011im"
translate "mov b, im" = "0111im"
translate "mov a, b" = "00010000"
translate "mov b, a" = "01000000"
translate "add a, im" = "0000im"
translate "add b, im" = "0101im"
translate "in a" = "00100000"
translate "in b" = "01100000"
translate "out im" = "1011im"
translate "out b" = "10010000"
translate "jmp im" = "1111im"
translate "jnc im" = "1110im"
