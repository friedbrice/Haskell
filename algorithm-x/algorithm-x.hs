import Data.List (subsequences)
import Control.Monad (guard)
import System.IO (getContents)

type Vector = [Int]
type Matrix = [Vector]
type LabelMatrix = [(Int, Vector)]

sumRows :: Matrix -> Vector
sumRows [] = repeat 0
sumRows (x:xs) = zipWith (+) x (sumRows xs)

label :: Matrix -> LabelMatrix
label = zip [1..]

unlabel :: LabelMatrix -> Matrix
unlabel = map snd

exactCovers :: Matrix -> [LabelMatrix]
exactCovers m = do
    let ones = replicate (length . head $ m) 1
    s <- subsequences . label $ m
    guard $ (sumRows . unlabel $ s) == ones
    return s

fromString :: String -> Matrix
fromString = map (map read) . map words . lines

main :: IO ()
main = print . exactCovers . fromString =<< getContents
