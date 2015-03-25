isPalindrome :: String -> String
isPalindrome s | s == reverse s = "palindrome"
               | otherwise      = "not a palindrome"

main :: IO ()
main = interact $ unlines . map (show . isPalindrome) . lines

