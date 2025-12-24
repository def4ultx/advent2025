import Data.List (partition, sortBy)
import Data.Ord (Down (..), comparing)
import Debug.Trace

import Data.Map (Map)
import Data.Map qualified as Map

type Vec3 = (Int, Int, Int)
type Pair = (Vec3, Vec3)
type Circuit = [Vec3]
type DistList = [(Pair, Int)]

toPair :: Vec3 -> Vec3 -> Pair
toPair a b
  | a < b = (a, b)
  | otherwise = (b, a)

parseLine :: String -> Vec3
parseLine line = do
  let (x, _ : rest) = break (== ',') line
  let (y, _ : z) = break (== ',') rest
  (read x :: Int, read y :: Int, read z :: Int)

sq :: Int -> Int
sq x = x * x

distSq :: Vec3 -> Vec3 -> Int
distSq (z1, y1, x1) (z2, y2, x2) = sq (z1 - z2) + sq (y1 - y2) + sq (x1 - x2)

allDistances :: [Vec3] -> [(Pair, Int)]
allDistances boxes = do
  let baseMap = foldl (\acc e -> Map.insert e Map.empty acc) Map.empty boxes
  let distMap =
        foldl
          ( \acc e ->
              let dists =
                    foldl
                      ( \accDists o ->
                          if e == o
                            then accDists
                            else Map.insert o (distSq e o) accDists
                      )
                      (acc Map.! e)
                      boxes
               in Map.insert e dists acc
          )
          baseMap
          boxes
  let distList = concatMap (\(a, dists) -> map (\(b, dist) -> (toPair a b, dist)) (Map.toList dists)) (Map.toList distMap)
  let uniqueDistList =
        Map.toList $
          foldl
            ( \acc (v, d) ->
                if Map.member v acc
                  then acc
                  else Map.insert v d acc
            )
            Map.empty
            distList
  -- sortBy (comparing snd) uniqueDistList
  sortBy (comparing (Down . snd)) uniqueDistList

findMinDistance :: DistList -> [Pair] -> (DistList, Pair)
findMinDistance linkOptions existingLinks = do
  let (alreadyLinked, options) = span (\(v, _) -> v `elem` existingLinks) linkOptions
  let td = snd $ head options
  let (matches, rest) = span (\(_, d) -> d == td) options
  case matches of
    [(x, _)] -> (rest, x)

merge :: Pair -> [Circuit] -> [Circuit]
merge (a, b) cs = do
  let ([cA], rest) = partition (a `elem`) cs
  if b `elem` cA
    then cs
    else do
      let ([cB], rest') = partition (b `elem`) rest
      (cA ++ cB) : rest'

showDists :: [(Pair, Int)] -> String
showDists [] = ""
showDists (((a, b), d) : ds) = show a ++ " -> " ++ show b ++ ": " ++ show d ++ "\n" ++ showDists ds

showCircuits :: [Circuit] -> String
showCircuits [] = ""
showCircuits (c : cs) = show c ++ "\n" ++ showCircuits cs

showEntry :: (Pair, Int) -> String
showEntry ((a, b), d) = show a ++ " -> " ++ show b ++ ": " ++ show d

connect :: DistList -> Int -> Int -> [Circuit] -> [Pair] -> [Circuit]
connect distances limit iteration circuits links =
  if iteration < limit
    then do
      let (distances', pair) = findMinDistance distances links
      let circuits' = merge pair circuits
      let links' = pair : links
      trace (show iteration ++ ": " ++ show (length circuits')) $ connect distances' limit (iteration + 1) circuits' links'
    else circuits

connectPair :: DistList -> Int -> Int -> [Circuit] -> [Pair] -> [Circuit]
connectPair distances limit iteration circuits links =
  if iteration < limit
    then do
      case distances of
        (pair:xs) ->
          let circuits' = merge pair circuits
          let links' = pair : links
          trace (show iteration ++ ": " ++ show (length circuits')) $ connectPair xs limit (iteration + 1) circuits' links'
        [] -> error "No more distances to process"
    else circuits


main :: IO ()
main = do
  content <- readFile "day08_input_example.txt"
  let linesOfFile = [l | l <- lines content, not (null l)]
  let boxes = map parseLine linesOfFile
  let distances = allDistances boxes
  -- _ <- putStrLn $ showDists distances
  let c = connect distances 1000 0 (map (: []) boxes) []
  _ <- putStrLn $ "Connected " ++ show (length c) ++ " circuits"
  let sizes = sortBy (comparing Down) $ map length c
  print $ product $ take 3 sizes
