-- | Type alias for Real-valued functions of a Real variable.
type Function = Double -> Double

-- | Returns Riemann sum using left endpoints.
leftRiemannSum :: Function  -- ^ Function to be integrated.
               -> Double    -- ^ Left boundary of integration.
               -> Double    -- ^ Right boundary of integration.
               -> Integer   -- ^ Number of rectangles.
               -> Double    -- ^ Approximate integral.
leftRiemannSum f a b n = sum ys * dx
  where
    dx = (b - a) / fromInteger n
    xs = map (\k -> a + fromInteger k * dx) [0..(n-1)]
    ys = map f xs

-- | Returns Riemann sum using right endpoints.
rightRiemannSum :: Function -- ^ Function to be integrated.
                -> Double   -- ^ Left boundary of integration.
                -> Double   -- ^ Right boundary of integration.
                -> Integer  -- ^ Number of rectangles.
                -> Double   -- ^ Approximate integral.
rightRiemannSum f a b n = sum ys * dx
  where
    dx = (b - a) / fromInteger n
    xs = map (\k -> a + fromInteger k * dx) [1..n]
    ys = map f xs
