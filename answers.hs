import qualified System.Random as SR

main :: IO ()
main = do
gen <- SR.newStdGen
print $ take 10 $ SR.randomRs ('a', 'd') gen

