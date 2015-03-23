---- linecount.hs
---- Counts the number of lines in a file.
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile)
import System.IO.Error (catchIOError)

countLines :: String -> String
countLines x = "The file has " ++ lineCount ++ " lines!"
	where lineCount = show . length . lines $ x

errorHandler :: IOError -> IO ()
errorHandler _ = putStrLn "The request was made, but it was no good." >> exitFailure

main :: IO ()
main = catchIOError main' errorHandler

main' :: IO ()
main' = putStrLn . countLines =<< readFile . head =<< getArgs
