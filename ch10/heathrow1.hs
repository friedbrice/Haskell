type Route = (String, Float)
type Step = (Float, Float, Float)

routing :: (Route, Route) -> Step -> (Route, Route)
routing ((aRt, aTm), (bRt, bTm)) (a,b,c) = ((aRt', aTm'), (bRt', bTm')) where
	(aRt', aTm') | aTm + a <= (bTm + b + c) = (aRt ++ "A" , aTm + a    )
	             | otherwise                = (bRt ++ "BC", bTm + b + c)
	(bRt', bTm') | bTm + b <= (aTm + a + c) = (bRt ++ "B" , bTm + b    )
	             | otherwise                = (aRt ++ "AC", aTm + a + c)

route :: [Step] -> Route
route steps | (snd . fst $ routes) <= (snd . snd $ routes) = fst routes
	        | otherwise                                    = snd routes
	where routes = foldl routing (("",0),("",0)) steps