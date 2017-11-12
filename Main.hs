{-# LANGUAGE DeriveDataTypeable #-}

import System.Directory
import System.Exit
import System.Environment
import System.IO
import Data.Char
import Data.IORef
import Control.Monad
import Text.Regex

import System.Console.CmdArgs
import qualified Machine.TD4 as TD4

data Machine = NoSelM | TD4 | Type1 | Type2 deriving (Data, Eq, Show)
data Direction = NoSelD | ASM | DisASM deriving (Data, Eq, Show)
data Verbose = OFF | ON deriving (Data, Show)

data CmdOpt = CmdOpt {
	machine :: Machine,
	direction :: Direction,
	file :: FilePath,
--	output :: FilePath,
	verbose :: Verbose
} deriving (Data, Show)

cmdOpt :: CmdOpt
cmdOpt = CmdOpt
	{
		machine	=	NoSelM
				&=	typ "{TD4 | Type1 | Type2}"
				&=	help "対象之計算機名"
				&=	groupname "換符ニ関為ル選項",
		direction	=	NoSelD
				&=	typ "{ASM | DisASM}"
				&=	help "換符之方向 (具意符→機械符: ASM, 具意符←機械符: DisASM)"
				&=	groupname "換符ニ関為ル選項",
		file 		=	def
				&=	argPos 0
				&=	typ "算譜名",
-- [要修正]: 出力先選択機能之追加
{-		output	=	def
				&=	opt ""
				&=	typ " 算譜名.変換先之拡張子 | _"
				&=	help "出力先之指定 (指定無キ時、入力為レシ算譜名之拡張子ヲ変更為タ物ト成ル。)"
				&=	groupname "換符ニ関為ル選項",
-}		verbose	=	OFF
				&=	opt "OFF"
				&=	typ " OFF | ON"
				&=	help "換符処理之表示ヲ行フ"
				&=	groupname "本算譜ニ対為ル選項"
	}
	&= helpArg [
				help "此文章ヲ表示為ル",
				groupname "本算譜ニ対為ル選項"
			   ]
	&= versionArg [
					help "本算譜之版ヲ表示為ル",
					groupname "本算譜ニ対為ル選項"
			        ]
	&= summary "Assembler 第〇.〇.一版"
	&= help "「神戸市立工業高等専門学校 電子計算機部 計算機製作プロジェクト」に於て製作為れし計算機群之換符系に候。\n"
	&= program "asm"
	&= details [
				"製作元: 神戸市立工業高等専門学校 電子計算機部 計算機製作プロジェクト",
				"製作場: https://github.com/orgs/KCCTdensan/teams/team-1/members"
			 ]

machineIsSelected :: CmdOpt -> Bool
machineIsSelected args = machine args /= NoSelM

directionIsSelected :: CmdOpt -> Bool
directionIsSelected args = direction args /= NoSelD

main = do
	args <- cmdArgs cmdOpt

	-- 選項之検査 --------------------------------------------------------------
	flag <- newIORef False

	unless (machineIsSelected args) $ do
		flag `writeIORef` True
		putStrLn "錯誤: 対象之計算機が指定為れて居ません。"
	unless (directionIsSelected args) $ do
		flag `writeIORef` True
		putStrLn "錯誤: 換符之方向が指定為れて居ません。"
	exist <- doesFileExist $ file args
	unless exist $ do
		flag `writeIORef` True
		putStrLn $ "錯誤: " ++ file args ++ " と云ふ算譜は存在為ません。"

	err <- readIORef flag
	when err $ do
		exitWith $ ExitFailure (-1)
	-----------------------------------------------------------------------------
{-
	inputFile <- head <$> getArgs

	let outputFile = head (splitRegex (mkRegex "[.]+") inputFile) ++ ".bin"
	ops <- lines <$> readFile inputFile
	writeFile outputFile (unlines $ assemble (machine args) ops) -- [要修正] 例外処理追加

assemble :: Machine -> [String] -> [String]
assemble TD4 ops = map TD4.assemble $ map (map toLower) ops
assemble Type1 ops =  error "非対応な計算機: type1"
assemble Type2 ops = error "非対応な計算機: type2"
-}
