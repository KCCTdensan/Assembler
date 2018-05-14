{-# LANGUAGE DeriveDataTypeable #-}

import System.Directory
import System.Exit
import System.Environment
import System.IO
import Data.IORef
import Control.Monad
import Data.Either
import Data.Char
import Text.Regex

import System.Console.CmdArgs
import qualified Machine.TD4 as TD4
-- import qualified Machine.Type1 as Type1
-- import qualified Machine.Type1 as Type2

-- [要調査及修正]: 言語擴張で、異なる代數的算料型に同名の値構成子を持たせられるのでは?
data Computer = NoSelC | TD4 | Type1 | Type2 deriving (Data, Eq, Show)
data ProcMode = NoSelM | ASM | DisASM deriving (Data, Eq, Show)
data Verbose = OFF | ON deriving (Data, Eq, Show)

data CmdOpt = CmdOpt {
	computer :: Computer,
	mode :: ProcMode,
	file :: FilePath,
	output :: FilePath,
	verbose :: Verbose
} deriving (Data, Show)

cmdOpt :: CmdOpt
cmdOpt = CmdOpt
	{
		computer =	NoSelC
				&=	typ "{TD4 | Type1 | Type2}"
				&=	help "対象ノ計算機名"
				&=	groupname "換符ニ関關爲ル選項",
		mode	=	NoSelM
				&=	typ "{ASM | DisASM}"
				&=	help "換符ノ方向 (具意符→機械符: ASM, 具意符←機械符: DisASM)"
				&=	groupname "換符ニ關爲ル選項",
		file 		=	def
				&=	argPos 0
				&=	typ "算譜名",
		output	=	def
				&=	opt ""
				&=	typ " (譜之本体名).{意符: asm, 械符: bin} | (指定値)"
				&=	help "書出先ノ指定 (指定無キ時、算譜ノ拡張子名ヲ変更爲タ物ト成ル。)"
				&=	groupname "換符ニ關爲ル選項",
		verbose	=	OFF
				&=	opt "OFF"
				&=	typ " OFF | ON"
				&=	help "換符處理ノ表示ヲ行フ"
				&=	groupname "本算譜ニ對爲ル選項"
	}
	&= helpArg [
				help "此文章ヲ表示爲ル",
				groupname "本算譜ニ對爲ル選項"
			   ]
	&= versionArg [
					help "本算譜ノ版ヲ表示爲ル",
					groupname "本算譜ニ對爲ル選項"
			        ]
	&= summary "Assembler 第〇.〇.一版"
	&= help "「神戸市立工業高等専門学校 電子計算機部 計算機製作プロジェクト」に於て製作爲れし計算機群之換符系に候。"
	&= program "asm"
	&= details [
				"",
				"例",
				"  $ asm -c=TD4 -m=asm Flash-LED.asm",
				"  $ asm -c TD4 -m asm -v=on sum.as -o=SUM.BIN",
				"  $ asm --computer=Type1 --mode=DisASM --verbose=ON Fibonacci.bin --output=Fib.asm",
				"",
				"",
				"製作元: 神戸市立工業高等専門学校 電子計算機部 計算機製作プロジェクト",
				"製作場: https://github.com/orgs/KCCTdensan/teams/team-1/members"
			 ]

computerIsSelected :: CmdOpt -> Bool
computerIsSelected args = computer args /= NoSelC

modeIsSelected :: CmdOpt -> Bool
modeIsSelected args = mode args /= NoSelM

verboseIsOn :: CmdOpt -> Bool
verboseIsOn args = verbose args == ON

main :: IO ()
main = do
	args <- cmdArgs cmdOpt

	-- 選項之検査 --------------------------------------------------------------
	flag <- newIORef False

	unless (computerIsSelected args) $ do
		flag `writeIORef` True
		putStrLn "錯誤: 對象の計算機が指定爲れてゐません。"
	unless (modeIsSelected args) $ do
		flag `writeIORef` True
		putStrLn "錯誤: 換符の方向が指定爲れてゐません。"
	exist <- doesFileExist $ file args
	unless exist $ do
		flag `writeIORef` True
		putStrLn $ "錯誤: " ++ file args ++ " と云ふ算譜は存在爲ません。"

	-- [要修正]: outputが若し指定爲れた場合を追加

	err <- readIORef flag
	when err $ do
		exitWith $ ExitFailure (-1)
	-----------------------------------------------------------------------------

	-- 換符処理 -----------------------------------------------------------------
	{- 書出先を設定 -}
	let outputFile =
		if output args == def
			then head (splitRegex (mkRegex "[.]+") $ file args) ++ "." ++ if mode args == ASM then "bin" else "asm"
			else output args
	
	{- 詳細書出 -}
	when (verboseIsOn args) $ do
		putStrLn $ "書出先: " ++ outputFile

	-- [以下要修正]: verbose=ONの反映, 錯誤時處理の追加, disassembleの實裝, 算帖處理の適正化, 文字列の読込→例: 文字每に處理
	insts <- map (map toLower) <$> lines <$> readFile (file args)

	{- 換符 -}
	let	result = convert (computer args) (mode args) insts
		converted = all isRight result
		codes = rights result
		errors = lefts result

	{- 錯誤書出 -}
	when (not converted) $ do
		mapM_ putStrLn errors

	{- 文字式帖方書出 -}
	-- [要修正]: 文字式算帖已成ら不、二進號式算帖にも對應爲べし。亦、夫は選項で指定爲べし
	when converted $ do
		writeFile outputFile $ unlines codes
	-----------------------------------------------------------------------------

convert ::  Computer -> ProcMode -> [String] -> [Either String String]
convert cmp ASM insts = assemble cmp insts
convert cmp DisASM insts = disassemble cmp insts

assemble :: Computer -> [String] -> [Either String String]
assemble TD4 insts = TD4.assemble insts
assemble cmp insts =  error $ "非對應な計算機: " ++ show cmp

disassemble :: Computer -> [String] -> [Either String String]
disassemble cmp insts = undefined
--disassemble TD4 insts = map TD4.disassemble $ map (map toLower) ops
--disassemble Type1 insts =  error "非對應な計算機: type1"
--disassemble Type2 insts = error "非對應な計算機: type2"
