import qualified Data.Set as Set

import Data.Set (Set)

import Data.List (sort, nub)

type Pos = (Int, Int)

data Side = North | East | South | West deriving (Show, Eq, Ord)

type EdgePos = (Side, Int)

type Atoms = [Pos]

type Interactions = [(EdgePos, Marking)]
data Marking = Absorb | Reflect | Path EdgePos deriving (Show, Eq)

--Challenge 1

getAdjacents :: Pos -> [Pos]
getAdjacents (c, r) = [(c-1, r), (c+1, r), (c, r-1), (c, r+1)]

add :: Pos -> (Int, Int) -> Pos
add (c, r) (dc, dr) = (c + dc, r + dr)

leftPerp, rightPerp :: (Int, Int) -> (Int, Int)
leftPerp (dc, dr)  = (-dr, dc)
rightPerp (dc, dr) = (dr, -dc)

getStartState :: Int -> EdgePos -> (Pos, (Int, Int))
getStartState n (North, c) = ((c, 0),   (0,  1))
getStartState n (South, c) = ((c, n+1), (0, -1))
getStartState n (East, r)  = ((n+1, r), (-1, 0))
getStartState n (West, r)  = ((0, r),   (1,  0))

isOutside :: Int -> Pos -> Bool
isOutside n (c,r) = c < 1 || c > n || r < 1 || r > n

posToEdgePos :: Int -> Pos -> EdgePos
posToEdgePos n (c,r)
  | r < 1     = (North, c)
  | r > n     = (South, c)
  | c < 1     = (West, r)
  | c > n     = (East, r)
  | otherwise = error $ "posToEdgePos: not an edge position " ++ show (c,r)

getFrontDiagonals :: Pos -> (Int, Int) -> (Pos, Pos)
getFrontDiagonals pos dir =
  let lp = leftPerp dir
      rp = rightPerp dir
      frontLeft  = add (add pos dir) lp
      frontRight = add (add pos dir) rp
  in (frontLeft, frontRight)


traceRay :: Int -> Set Pos -> EdgePos -> Pos -> (Int, Int) -> Set (Pos,(Int,Int)) -> Marking
traceRay n atomSet entryPos currentPos dir visited =
  let nextPos = add currentPos dir
      state   = (currentPos, dir) 
  in if isOutside n nextPos
       then let exitEdge = posToEdgePos n nextPos
            in if exitEdge == entryPos then Reflect else Path exitEdge
       else if Set.member nextPos atomSet
            then Absorb
            else 
              if state `Set.member` visited then Reflect
              else
                let (fl, fr) = getFrontDiagonals currentPos dir 
                    hasFL = Set.member fl atomSet
                    hasFR = Set.member fr atomSet
                    visited' = Set.insert state visited
                in case (hasFL, hasFR) of
                     (True, True)   -> Reflect 
                     (True, False)  -> traceRay n atomSet entryPos currentPos (rightPerp dir) visited' 
                     (False, True)  -> traceRay n atomSet entryPos currentPos (leftPerp dir) visited'
                     (False, False) -> traceRay n atomSet entryPos nextPos dir visited'


fireRay :: Int -> Set Pos -> EdgePos -> (EdgePos, Marking)
fireRay n atomSet entryPos =
  let 
    (startPos, dir) = getStartState n entryPos
    
    firstStepPos = add startPos dir
  in
    if Set.member firstStepPos atomSet then
      (entryPos, Absorb)
      
    else
      let 
        adjacentsToFirst = getAdjacents firstStepPos
        
        nextStepPos = add firstStepPos dir
        
        sideAtoms = filter (\pos -> pos /= nextStepPos) adjacentsToFirst
        
        isImmediateReflect = any (`Set.member` atomSet) sideAtoms
      in
      if isImmediateReflect then
        (entryPos, Reflect)
        
      else
        (entryPos, traceRay n atomSet entryPos startPos dir Set.empty)


calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions n atoms =
  let allEntries = map (North,) [1..n] ++ map (East,) [1..n] ++
                   map (South,) [1..n] ++ map (West,) [1..n]
      atomSet = Set.fromList atoms
  in nub $ map (fireRay n atomSet) allEntries


--challenge 2
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

inferGridSize :: Interactions -> Int
inferGridSize [] = 0 
inferGridSize interactions = 
    let indices = map (\((_, i), _) -> i) interactions
    in maximum indices

verifyCandidate :: Int -> Set Pos -> Interactions -> Bool
verifyCandidate gridSize atomSet observedInteractions =
    all matches observedInteractions
  where
    matches (startPos, expectedMarking) =
        let (_, actualMarking) = fireRay gridSize atomSet startPos
        in actualMarking == expectedMarking


solveBB :: Int -> Int -> Interactions -> [Atoms]
solveBB sizeInput numAtoms interactions =
    let 
        maxInteractionIndex = inferGridSize interactions
    in
        if maxInteractionIndex > sizeInput 
        then error ("Input Error: Interaction index " ++ show maxInteractionIndex ++ 
                    " is larger than grid size " ++ show sizeInput)
        else
            let 
                gridSize = sizeInput
                allCoords = [(c, r) | c <- [1..gridSize], r <- [1..gridSize]]
                candidateAtoms = combinations numAtoms allCoords
            in
                filter (\atoms -> verifyCandidate gridSize (Set.fromList atoms) interactions) candidateAtoms



main :: IO ()
main = do 
    -- print (calcBBInteractions 8 [(2,3),(7,3),(4,6),(7,8)])
    print (solveBB 8 4 [((North,1),Path (West,2)),((North,2),Absorb),((North,3),Path (North,6)),((North,4),Absorb),((North,5),Path (East,5)),((North,6),Path (North,3)),((North,7),Absorb),((North,8),Path (East,2)),((East,1),Path (West,1)),((East,2),Path (North,8)),((East,3),Absorb),((East,4),Path (East,7)),((East,5),Path (North,5)),((East,6),Absorb),((East,7),Path (East,4)),((East,8),Absorb),((South,1),Path (West,4)),((South,2),Absorb),((South,3),Path (West,7)),((South,4),Absorb),((South,5),Path (West,5)),((South,6),Reflect),((South,7),Absorb),((South,8),Reflect),((West,1),Path (East,1)),((West,2),Path (North,1)),((West,3),Absorb),((West,4),Path (South,1)),((West,5),Path (South,5)),((West,6),Absorb),((West,7),Path (South,3)),((West,8),Absorb)])
    print (solveBB 8 1 [((North,1), Absorb)])
    print (solveBB 8 1 [((North, 3), Absorb), ((West, 3), Absorb)])
    print (solveBB 8 1 [((North, 2), Path (West, 1))])
    print (solveBB 8 1 [((North, 1), Absorb), ((South, 1), Path (North, 1))])
    print (solveBB 4 1 [((North, 2), Absorb), ((West, 2), Absorb)])
    print (solveBB 10 1 [((North, 9), Absorb), ((West, 9), Absorb)])
    print (solveBB 3 1 [((North, 5), Absorb)])




