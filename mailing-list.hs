geographys = [ "Souther California"
             , "California"
             , "West Coast"
             , "Western"
             ]

subjects = [ "Mathematics"
           , "Algebra"
           , "Linear Algebra"
           , "Lie Theory"
           , "Lie Algebras"
           , "Lie Groups"
           , "Representation Theory"
           , "Matrix"
           , "Category Theory"
           , "Homological Algebra"
           ]

searches = [i ++ " " ++ j ++ " mailing list" | i <- geographys, j <- subjects]

main = mapM_ print searches

