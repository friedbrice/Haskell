import Data.List (delete)

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("view", view')
           , ("add", add')
           , ("remove", remove')
           , ("bump", bump')
           ]

type Task = (Int, String)
type Todo = [Task]

main :: IO ()
main = undefined

view :: Todo -> String
view [] = ""
view ((num,task):tasks) = show num ++ "\t" ++ show task ++ "\n" ++ view tasks
view' = undefined

add :: Todo -> Task -> Todo
add todo task = todo ++ [task]
add' = undefined

remove :: Todo -> Task -> Todo
remove todo task = delete task todo
remove' = undefined

bump :: Todo -> Task -> Todo
bump todo task = task : delete task todo
bump' = undefined
