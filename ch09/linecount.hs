---- linecount.hs
---- Counts the number of lines in a file.
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile)
import System.IO.Error
	( catchIOError
	, ioeGetFileName
	, ioError
	, isDoesNotExistError
	, isPermissionError
	)

countLines :: String -> String
countLines x = "The file has " ++ lineCount ++ " lines!"
	where lineCount = show . length . lines $ x

errorHandler :: IOError -> IO ()
errorHandler e
	| isDoesNotExistError e = putStrLn $ "File does not exist: " ++ path
	| isPermissionError e   = putStrLn $ "Permission denied: " ++ path
	| otherwise             = ioError e
	where
		path = case ioeGetFileName e of
			Just path -> path
			Nothing   -> "unknown path"

main :: IO ()
main = catchIOError main' (\e -> errorHandler e >> exitFailure)

main' :: IO ()
main' = putStrLn . countLines =<< readFile . head =<< getArgs
