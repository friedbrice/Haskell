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

-- | Converts two points with derivative to QBezier notation.
splineToQbez :: (Float, Float, Float) -- ^ First point, with slope
             -> (Float, Float, Float) -- ^ Second point, with slope
             -> [(Float, Float)]      -- ^ Curve in QBez notation.
splineToQbez (x0,y0,s0) (x2,y2,s2) = [(x0,y0),(x1,y1),(x2,y2)]
  where
    x1 = (s0 * x0 - s2 * x2 - y0 + y2) / (s0 - s2)
    y1 = s0 * (x1 - x0) + y0

-- | Console wrapper for quadToQbez.
main :: IO ()
main = print . unQuadToQbez . toTuple . map read =<< getArgs
  where
    unQuadToQbez = uncurry . uncurry $ quadToQbez
    toTuple (a:b:c:[]) = ((a, b), c)
