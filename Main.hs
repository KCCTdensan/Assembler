{-# LANGUAGE DeriveDataTypeable #-}

import System.Directory
import System.Exit
import System.Environment
import System.IO
import Data.Char
import Data.IORef
import Control.Monad
import Text.Regex

import System.Console.CmdArgs hiding (Mode)
import qualified Machine.TD4 as TD4

data Computer = NoSelC | TD4 | Type1 | Type2 deriving (Data, Eq, Show)
data Mode = NoSelD | ASM | DisASM deriving (Data, Eq, Show)
data Verbose = OFF | ON deriving (Data, Show)

data CmdOpt = CmdOpt {
	computer :: Computer,
	mode :: Mode,
	file :: FilePath,
	output :: FilePath,
	verbose :: Verbose
} deriving (Data, Show)

cmdOpt :: CmdOpt
cmdOpt = CmdOpt
	{
		computer =	NoSelC
				&=	typ "{TD4 | Type1 | Type2}"
				&=	help "対象之計算機名"
				&=	groupname "換符ニ関為ル選項",
		mode	=	NoSelD
				&=	typ "{ASM | DisASM}"
				&=	help "換符之方向 (具意符→機械符: ASM, 具意符←機械符: DisASM)"
				&=	groupname "換符ニ関為ル選項",
		file 		=	def
				&=	argPos 0
				&=	typ "算譜名",
		output	=	def
				&=	opt ""
				&=	typ " (譜之本体名).{意符: asm, 械符: bin} | (選項値)"
				&=	help "出力先之指定 (指定無キ時、算譜之拡張子名ヲ変更為タ物ト成ル。)"
				&=	groupname "換符ニ関為ル選項",
		verbose	=	OFF
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

computerIsSelected :: CmdOpt -> Bool
computerIsSelected args = computer args /= NoSelC

modeIsSelected :: CmdOpt -> Bool
modeIsSelected args = mode args /= NoSelD

main = do
	args <- cmdArgs cmdOpt

	-- 選項之検査 --------------------------------------------------------------
	flag <- newIORef False

	unless (computerIsSelected args) $ do
		flag `writeIORef` True
		putStrLn "錯誤: 対象之計算機が指定為れて居ません。"
	unless (modeIsSelected args) $ do
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

	-- 換符処理 -----------------------------------------------------------------
	-- [要修正]: 選項之反映
	let inputFile = file args
	let outputFile = head (splitRegex (mkRegex "[.]+") inputFile) ++ ".bin"
	ops <- lines <$> readFile inputFile
	writeFile outputFile (unlines $ assemble (computer args) ops) -- [要修正] 例外処理之追加
	-----------------------------------------------------------------------------

assemble :: Computer -> [String] -> [String]
assemble TD4 ops = map TD4.assemble $ map (map toLower) ops
assemble Type1 ops =  error "非対応な計算機: type1"
assemble Type2 ops = error "非対応な計算機: type2"
