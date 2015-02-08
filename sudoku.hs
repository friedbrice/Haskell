-- modified from https://www.haskell.org/pipermail/haskell-cafe/2006-August/017309.html
import Data.List (lines, unlines, words, unwords, delete)
-- We're using Data.List mostly for string manipulation.
-- Their usages are clear from their names and types.
import System.IO (getContents, putStr)
-- Again, usage is clear from name and type.

type T = (Int,Int) -> [Int]
-- A T is a function from indicies to a list of possible entries.
-- Intuitively, it's an array of possible ways to complete a sudoku.

idx :: [(Int,Int)]
-- Global constant, list of indicies.
-- idx is the domain of a T.
idx = [(i,j) | i <- [1..9], j <- [1..9]]

myInit :: T
-- Global constant, initial value for foldr mark.
-- myInit (i,j) = [1..9], no matter what i and j are,
-- i.o.w., no information about what each entry might be.
myInit = const [1..9]

input :: String -> T
-- Turns a starting sudoku into a function of possible entries.
-- mark does the real work of parring down possible entries.
input s = foldr mark myInit $
  [(p,n) | (p,n) <- zip idx $ map read $ lines s >>= words, n>0]
  -- "[(p,n) | ...]" is an [((Int,Int),Int)], by the way
  -- p stands for "position", n stands for "number"
  -- "zip idx $ map read $ lines s >>= words" is an [((Int,Int),b)]
  -- is turn your input array into a matrix (ie, function on idx)

sameBlock :: (Int,Int) -> (Int,Int) -> Bool
-- Tells you if two positions are in the same 3 by 3 block.
-- Used in mark.
sameBlock (i,j) (x,y) = e x i && e y j
  where e a b = div (a-1) 3 == div (b-1) 3

mark :: ((Int,Int),Int) -> T -> T
-- Takes data from our input sudoku (first arg),
-- and an intermediate/heuristic solution (second arg),
-- and returns a refinement of the given (return).
mark (p@(i,j),n) s q@(x,y) =
-- p is shorthand for (i,j)
-- q is shorthand for (x,y)
-- that's all the @ keyword does
-- 'mark (p,n) s' is a function
-- we define it's return for a representative arg, q
  if p==q then [n] else
  -- if position p is already filled in, leave it alone
  if x==i || y==j || sameBlock p q then delete n . s $ q else s q
  -- if we already have n in a conflicting position, n can't be in q

solve :: [T] -> [T]
solve s = foldr search s idx where
    search p l = [mark (p,n) s | s <- l, n <- s p]

disp :: T -> String
disp s  = unlines [unwords [show $ head $ s (i,j) | j <- [1..9]] | i <- [1..9]]

main :: IO ()
main = do
  s <- getContents
  putStr . unlines . map disp . solve $ [input s]

