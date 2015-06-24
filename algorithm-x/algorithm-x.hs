import Data.List (subsequences)
import Control.Monad (guard)
import System.IO (getContents)

type Vector = [Int]
type Matrix = [Vector]
type LabelMatrix = [(Int, Vector)]

fromString :: String -> Matrix
fromString = map (map read . words) . lines

label :: Matrix -> LabelMatrix
label = zip [1..]

unlabel :: LabelMatrix -> Matrix
unlabel = map snd

sumRows :: Matrix -> Vector
sumRows = foldr (zipWith (+)) (repeat 0)

exactCovers :: Matrix -> [LabelMatrix]
exactCovers m = do
    let ones = replicate (length . head $ m) 1
    s <- subsequences . label $ m
    guard $ (sumRows . unlabel $ s) == ones
    return s

main :: IO ()
main = print . exactCovers . fromString =<< getContents
