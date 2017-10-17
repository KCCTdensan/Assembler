import System.Environment
import System.IO
import Text.Regex
import Data.Char

import qualified Machine.TD4 as TD4

data Machine = TD4 | Type1

main = do
	-- [要修正] 選項解析処理及夫錯誤時之処理
	inputFile <- head <$> getArgs
	let machine = TD4
	-- 此処迄

	let outputFile = head (splitRegex (mkRegex "[.]+") inputFile) ++ ".bin"
	ops <- lines <$> readFile inputFile
	writeFile outputFile (unlines $ assemble machine ops) -- [要修正] 例外処理追加

help :: String
help = "" -- [要修正]

assemble :: Machine -> [String] -> [String]
assemble TD4 ops = map TD4.assemble $ map (map toLower) ops
assemble Type1 ops = ["Sorry, this machine is unsupported yet."]
