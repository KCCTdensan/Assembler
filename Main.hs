import System.Environment
import System.IO

main = do
	file <- head <$> getArgs
	lines <$> readFile file >>= print

help :: String
help = ""
