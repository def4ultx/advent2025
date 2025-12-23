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
  sortBy (comparing (Down . fst . fst)) uniqueDistList

merge :: Pair -> [Circuit] -> [Circuit]
merge (a, b) cs = do
  let ([cA], rest) = partition (a `elem`) cs
  if b `elem` cA
    then cs
    else do
      let ([cB], rest') = partition (b `elem`) rest
      (cA ++ cB) : rest'

connectPair :: DistList -> Int -> Int -> [Circuit] -> [Pair] -> [Circuit]
connectPair distances limit iteration circuits links =
  if iteration < limit
    then do
      case distances of
        ((pair, _):xs) -> connectPair xs limit (iteration + 1) (merge pair circuits) links
  else circuits

myLongString :: String
myLongString = concat [
  "162,817,812",
  "57,618,57",
  "906,360,560",
  "592,479,940",
  "352,342,300",
  "466,668,158",
  "542,29,236",
  "431,825,988",
  "739,650,466",
  "52,470,668",
  "216,146,977",
  "819,987,18",
  "117,168,530",
  "805,96,715",
  "346,949,466",
  "970,615,88",
  "941,993,340",
  "862,61,35",
  "984,92,344",
  "425,690,689"
  ]

main :: IO ()
main = do
  let linesOfFile = [l | l <- lines myLongString, not (null l)]
  let boxes = map parseLine linesOfFile
  let distances = allDistances boxes
  let c = connectPair distances 1000 0 (map (: []) boxes) []
  _ <- putStrLn $ "Connected " ++ show (length c) ++ " circuits"
  let sizes = sortBy (comparing Down) $ map length c
  print $ product $ take 3 sizes
