import System.IO (readFile)
import System.Environment (getArgs)

main :: IO ()
main = do
  paths <- getArgs
  conts <- mapM readFile paths
  let cat = concat conts
  putStr cat

