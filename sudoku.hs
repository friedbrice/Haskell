-- modified from https://www.haskell.org/pipermail/haskell-cafe/2006-August/017309.html
import Data.List (lines, unlines, words, unwords, delete)
-- we're using Data.List mostly for string manipulation
-- their usages are clear from their names and types
import System.IO (getContents, putStr)
-- again, usage is clear from name and type

type T = (Int,Int) -> [Int]
-- a T is a function from indicies to a list of possible entries
-- intuitively, it's an array of possible ways to complete a sudoku

idx :: [(Int,Int)]
-- global constant, list of indicies
-- a T has domain idx
idx = [(i,j) | i <- [1..9], j <- [1..9]]

myInit :: T
-- global constant, initial value for foldr mark
-- myInit (i,j) = [1..9], no matter what i and j are
-- i.o.w., no information about what each entry might be
myInit = const [1..9]

input :: String -> T
-- turns a starting sudoku into an array of possible completions
-- mark does the real work of parring down possible entries
input s = foldr mark myInit $
  [(p,n) | (p,n) <- zip idx $ map read $ lines s >>= words, n>0]
  -- [(p,n)] is an [(Int,Int),Int], by the way
  -- p stands for "position", n stands for "number"

mark :: ((Int,Int),Int) -> T -> T
mark (p@(i,j),n) s q@(x,y) =
-- p is shorthand for (i,j)
-- q is shorthand for (x,y)
-- that's all the @ keyword does
  if p==q then [n] else
  if x==i || y==j || e x i && e y j then delete n $ s q else s q
  where e a b = div (a-1) 3==div (b-1) 3

solve :: [T] -> [T]
solve s = foldr search s idx where
    search p l = [mark (p,n) s | s <- l, n <- s p]

disp :: T -> String
disp s  = unlines [unwords [show $ head $ s (i,j) | j <- [1..9]] | i <- [1..9]]

main :: IO ()
main = do
  s <- getContents
  putStr . unlines . map disp . solve $ [input s]

