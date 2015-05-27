-- quadratic-to-qubezier.hs
import System.Environment (getArgs)

-- | Converts y = Ax^2 + Bx + C to QBezier notation.
quadToQbez :: Float             -- ^ Coefficient of x^2.
           -> Float             -- ^ Coefficient of x.
           -> Float             -- ^ Constant term.
           -> [(Float, Float)]  -- ^ Curve in QBez notation.
quadToQbez a b c = [(x0, y0), (x1, y1), (x2, y2)]
  where
	x0   = -b / (2 * a) - 1
	y0   = f x0
	x1   = -b / (2 * a)
	y1   = f x0 + f' x0
	x2   = -b / (2 * a) + 1
	y2   = f x2
	f  x = a * x^2 + b * x + c
	f' x = 2 * a * x + b

-- | Console wrapper for quadToQbez.
main :: IO ()
main = print . unQuadToQbez . toTuple . map read =<< getArgs
  where
    unQuadToQbez = uncurry . uncurry $ quadToQbez
    toTuple (a:b:c:[]) = ((a, b), c)
