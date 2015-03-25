---- monocat.hs
---- A software tool for concatenating a single file.
import System.IO (readFile)
import System.Environment (getArgs)

main :: IO ()

--- Using `do` ---
--main = do
--  (path:_) <- getArgs
--  contents <- readFile path
--  putStr contents

--- Using `>>=` ---
--main = fmap head getArgs >>= readFile >>= putStr

--- Using `>>=` with `.` ---
--main = getArgs >>= readFile . head >>= putStr

--- Using `=<<` ---
main = putStr =<< readFile . head =<< getArgs

--- Using `>>=` with `#` ---
--main = getArgs >>= head # readFile >>= putStrLn

