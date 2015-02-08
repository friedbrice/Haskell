-- modified from https://www.haskell.org/pipermail/haskell-cafe/2006-August/017309.html
-- modified to solve 6 by 6 sudoku in 2-row by 3-col blocks
import Data.List (lines, unlines, words, unwords, delete)
-- we're using Data.List mostly for string manipulation.
-- their usages are clear from their names and types.
import System.IO (getContents, putStr)
-- again, usage is clear from name and type.

type T = (Int,Int) -> [Int]
-- a T is a function from indicies to a list of possible entries.
-- intuitively, it's an array of possible ways to complete a sudoku.

idx :: [(Int,Int)]
-- global constant, list of indicies.
-- a T has domain idx.
idx = [(i,j) | i <- [1..6], j <- [1..6]]

myInit :: T
-- global constant, initial value for foldr mark.
-- myInit (i,j) = [1..9], no matter what i and j are,
-- i.o.w., no information about what each entry might be.
myInit = const [1..6]

input :: String -> T
-- turns a starting sudoku into a function of possible entries.
-- mark does the real work of parring down possible entries.
input s = foldr mark myInit $
  [(p,n) | (p,n) <- zip idx $ map read $ lines s >>= words, n>0]
  -- [(p,n) | ...] :: [((Int,Int),Int)]
  -- p stands for "position", n stands for "number".
  -- zip idx $ map read $ lines s >>= words :: [((Int,Int),Int)]
  -- represents your input array as ordered pairs on domain idx.

sameBlock :: (Int,Int) -> (Int,Int) -> Bool
-- tells you if two positions are in the same 2 by 3 block,
-- used in mark.
sameBlock (i,j) (x,y) =
  div (x-1) 2 == div (i-1) 2 && div (y-1) 3 == div (j-1) 3

mark :: ((Int,Int),Int) -> T -> T
-- takes data from our input sudoku (first arg),
-- and an intermediate/heuristic solution (second arg),
-- and returns a refinement of the given (return).
mark (p@(i,j),n) s q@(x,y) =
-- p is shorthand for (i,j),
-- q is shorthand for (x,y),
-- that's all the @ keyword does.
-- "mark (p,n) s" is a function, from (Int,Int) to [Int],
-- we define it's return for a representative arg, namely "q".
  if p == q then [n] else
  -- if position q is already known, leave it alone.
  if x == i || y == j || sameBlock p q then delete n . s $ q
  -- if we already have n in a conflicting position,
  -- then n can't be in position q, remove it as a possibility.
  else s q
  -- we got no new info this pass, leave q alone.

solve :: [T] -> [T]
-- really just iterates mark.
solve s = foldr search s idx
-- search is the function we're folding,
-- idx is what we're folding over,
-- s is foldr's init seed. s :: [T]
  where search p l = [mark (p,n) s | s <- l, n <- s p]
  -- search :: (Int, Int) -> [T] -> [T]
  -- p is for "position". p :: (Int,Int)
  -- l is for "list". l :: [T]

-- it seems redundant that both input and solve call mark.

disp :: T -> String
disp s  = unlines [unwords [show $ head $ s (i,j) | j <- [1..6]] | i <- [1..6]]

main :: IO ()
main = do
  s <- getContents
  putStr . unlines . map disp . solve $ [input s]

