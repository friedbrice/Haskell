import System.Environment (getArgs)

main :: IO ()
main = putStrLn . (++) "Hello, " . head =<< getArgs

