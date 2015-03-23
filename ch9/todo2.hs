import Data.List (delete)

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("view", view')
           , ("add", add')
           , ("remove", remove')
           , ("bump", bump')
           ]

Type Task = (Int, String) deriving show read
Type Todo = [Task] deriving show read

main :: IO ()

view :: Todo -> String
view [] = ""
view ((num,task):tasks) = show num ++ "\t" ++ show task ++ "\n" ++ view tasks

add :: Todo -> Task -> Todo
add todo task = todo ++ [task]

remove :: Todo -> Task -> Todo
remove todo task = delete task todo

bump :: Todo -> Task -> Todo
bump todo task = [task] ++ (delete task todo)

