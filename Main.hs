import Control.Monad
import System.Directory
import System.Environment
import System.Exit

-- [要󠄁]: コマンドライン引數の處理に就て、ライブラリ(例: CmdArgs)を使ふ等
-- [要󠄁]: 複數の計算機に對應
-- [要󠄁]: 逆アセンブルに對應
main :: IO ()
main = do
	-- コマンドライン引數 ----------------------------------------------------------------------
	args <- getArgs
	let	ipf	= head args		-- 入力元
		opf	= (head . tail) args	-- 出力先
	putStrLn $ ipf ++ " → " ++ opf
	----------------------------------------------------------------------------------------------
	
	-- アセンブル -------------------------------------------------------------------------------
	result <- asmbl ipf opf

	unless result $ do
		putStrLn "[Error]: アセンブル失敗"
		exitWith $ ExitFailure (-1)
	----------------------------------------------------------------------------------------------

-- [要󠄁]: 動く樣に
-- [要󠄁]: エラー文章も得られる樣に
asmbl :: FilePath -> FilePath -> IO Bool
asmbl ipf opf = do
	-- 入力元が存在しない場合
	fileExist <- doesFileExist ipf
	unless fileExist $ do
		putStrLn $ "[Error]: " ++ ipf ++ "は存在しません"
		return False

	result <- (unlines . toMachineCodes . lines) <$> readFile ipf
	if (isJust result)
		then writeFile opf result
		else Nothing
	return True

type ASMCode	= String
type MachineCode	= String

-- [要󠄁]: エラー文章も得られる樣に
toMachineCodes :: [ASMCode] -> Maybe [MachineCode]
toMachineCodes asmcds = sequence $ map toMachineCode asmcds

-- [要󠄁]: 實裝
-- [要󠄁]: エラー文章も得られる樣に
toMachineCode :: ASMCode -> Maybe MachineCode
toMachineCode asmcd = Nothing
