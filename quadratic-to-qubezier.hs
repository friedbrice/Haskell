-- quadratic-to-qubezier.hs
import System.Environment

quadToQbez :: [Float] -> [(Float, Float)]
quadToQbez (a:b:c:cs) = [(x0, y0), (x1, y1), (x2, y2)] where
	x0 = -b / (2 * a) - 1
	y0 = f x0
	x1 = -b / (2 * a)
	y1 = f x0 + f' x0
	x2 = -b / (2 * a) + 1
	y2 = f x2
	f  = (\x -> a * x^2 + b * x + c)
	f' = (\x -> 2 * a * x + b)

main :: IO ()
main = print . quadToQbez . map read =<< getArgs
