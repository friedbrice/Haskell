module Birds
( Birds(..)
, Pole(..)
, landLeft
, landRight
) where

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (l, r) = (l + n, r)

landRight :: Birds -> Pole -> Pole
landRight n (l, r) = (l, r + n)
