import Data.Char

-- Usage:
-- Linux/Mac:
--   cat depths.txt  | depth
-- Windows:
--   type depths.txt | depth.exe

toInt :: String -> Int
toInt = read

data Difference = Lesser Int | Greater Int | NoChange deriving (Show)

deriv :: [Int] -> [Difference]
deriv (first : second : rest) 
  | first < second  = Greater (second - first) : deriv (second : rest)
  | first > second  = Lesser (first - second)  : deriv (second : rest)
  | first == second = NoChange : deriv (second : rest)
deriv _ = []

runningTotal :: [Int] -> [Int]
runningTotal (d1 : d2 : d3 : rest) = (d1 + d2 + d3) : runningTotal (d2 : d3 : rest)
runningTotal _ = []



main = do
  depthStr <- getContents
  let runningDepth = runningTotal $ map toInt $ lines depthStr
  let changes = deriv runningDepth
  let timesLarger = foldl (\a b -> case b of 
                                    (Greater _) -> a + 1
                                    (Lesser _)  -> a
                                    NoChange    -> a) 
                         0 changes
  putStr $ show timesLarger