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

assemble :: Machine -> [String] -> [String]
assemble TD4 ops = map TD4.assemble $ map (map toLower) ops
assemble Type1 ops = ["Sorry, this machine is unsupportedyet."]

help :: String
help = "---------- Assembler 第0.0.1版 ----------\n" ++
	"\n" ++
	"    [説明]\n" ++
	"        アセンブル及び逆アセンブルを行う算譜です。\n" ++
	"\n" ++
	"    [使い方]\n" ++
	"        asm -[選項] (ファイル名)\n" ++
	"\n" ++
	"        <選項一覧>\n" ++
	"            t (計算機名): 対象を指定する\n" ++
	"                計算機名: td4, ...\n" ++
	"            r: 逆アセンブルを行う\n" ++
	"\n" ++
	"        <例>\n" ++
	"            asm -t td4 program.asm\n" ++
	"            asm -t td4 -r program.bin\n" ++
	"\n" ++
	"    [情報]\n" ++
	"        制作元: 神戸高専 電子計算機部 計算機製作プロジェクト\n" ++
	"        製作場: https://github.com/orgs/KCCTdensan/teams/team-1/members"
