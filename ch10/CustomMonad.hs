module CustomMonad
( (#)
, (<<)
) where

infixl 0 #
(#) = flip (.)

infixr 1 <<
(<<) = flip (>>)
