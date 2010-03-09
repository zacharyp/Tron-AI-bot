import System.IO
import Data.List
import Debug.Trace

setBuffers = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

main = playBot myBot startingValue

playBot :: ([[Spot]] -> a -> (Move, a)) -> a -> IO ()
playBot bot starting = do
    setBuffers
    interact ((playTurns bot starting) . lines)

readInt :: String -> Int
readInt a = read a

readSpot '#' = Wall
readSpot ' ' = Blank
readSpot '1' = Player
readSpot '2' = Enemy

makeMove North = "1"
makeMove East = "2"
makeMove South = "3"
makeMove West = "4"

playTurns bot pastValue [] = ""
playTurns bot pastValue str = (makeMove move) ++ "\n" ++ playTurns bot history (drop (h+1) str)
    where [w,h] = map readInt (words $ head str)
          tronMap = map (map readSpot) (take h (tail str))
	  (move, history) = bot tronMap pastValue

data Spot = Wall | Blank | Player | Enemy deriving Eq
data Move = North | East | South | West deriving (Eq,Show)

startingValue = ()

me tronMap = (maybe 0 id (findIndex (== Player) (head $ filter (any (== Player)) tronMap)), maybe 0 id (findIndex (any (== Player)) tronMap))

them tronMap = (maybe 0 id (findIndex (== Enemy) (head $ filter (any (== Enemy)) tronMap)), maybe 0 id (findIndex (any (== Enemy)) tronMap))


canMove :: Move -> Spot -> (Int, Int) -> [[Spot]] -> Bool
canMove move s (x,y) tronMap
    | move == North	= if y == 0 then False else (s == ((tronMap !! (y-1)) !! x))
    | move == East	= if x+1 == (length (head tronMap)) then False else (s == ((tronMap !! y) !! (x+1)))
    | move == South	= if y+1 == (length tronMap) then False else (s == ((tronMap !! (y+1)) !! x))
    | move == West	= if x == 0 then False else (s == ((tronMap !! y) !! (x-1)))





----------------------- {- New code -} ------------------------------------

{-
  The function that is called by the server-side interface code above.  It is 
  passed a tron map as a list of list of Spots (walls, blank spaces, a player,
  and an enemy).  Should return a pair of the next move to be made by the player,
  with the second in the pair simply being the first argument passed in.
-}
myBot :: [[Spot]] -> a -> (Move, a)
myBot tronMap b = (head $ step3 tronMap, b )


step3 tM | length s2 > 0 = [ head s2 ]
         | otherwise = step3Decide tM ++ bestMove tM ++ enemyTo tM ++ [North] 
      where s2 = step2 tM

bestMove tronMap = (filter (\a -> canMove a Blank (me tronMap) tronMap) [North, East, South, West])

enemyTo tronMap = (filter (\a -> canMove a Enemy (me tronMap) tronMap) [North, East, South, West])

step2 tronMap | length s1 < 2  = []
              | length s1 == 2 = nextToDecide tronMap
              | length s1 == 3 = minimaxDecide tronMap
              | length s1 < 8  = floodFillDecide tronMap
              | otherwise      = aMove
      where s1 = step1 tronMap
            aMove = [ (locDiff (me tronMap) (last (init s1))) ]

step1 tronMap = if (length findPath < 2)
                   then []
                   else findPath
   where findPath = aStar tronMap (me tronMap) (them tronMap)


{- Type for holding x and y cordinates of a vertex.  -}
type Vertex = (Int, Int)

{-
  Simple function to return the cardinal direction to move to get from one
  vertext to another.
-}
locDiff:: Vertex -> Vertex -> Move
locDiff (x,y) (x2,y2)
  | y<y2 = South
  | y>y2 = North 
  | x<x2 = East
  | x>x2 = West
  | otherwise = North






{-
  Modified BFS, using a Manhattan distance heuristic to add to the pending list.  New
  nodes are added to the pending list by inserting it into the correct sorted position
  by Manhattan distance.  This way, closer vertices are followed before vertices on
  the far side of the current vertex away from the destination.  The far side child
  vertex are added to the end of the pending list, so that they will get checked
  eventually if the close side search doesn't find the destination.
-}
aStar :: [[Spot]] -> Vertex -> Vertex -> [Vertex]
aStar tronMap source dest = help [(source,[source],0)] []
  where help [] _ = []
        help ((p,ps,mDis):pending) visited 
             | p == dest = ps
             | otherwise = help (add children pending) (p:visited ++ children)
            where children = adjOpenVert tronMap p
                  add [] qs = qs
                  add (c:cs) qs 
                    | elem c visited = add cs qs
                    | otherwise = add cs ( sortBy mDistSort ((c, c:ps, (manhDist dest c) ):qs))

{-
  Returns a list of the open vertices adjacent to the passed vertex.
-}
adjOpenVert:: [[Spot]] -> Vertex -> [Vertex]
adjOpenVert t (x,y) = [ a | a <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)] , isOpen t a ]
       where isOpen t (x,y) = any (== getSpot) [Blank,Enemy]
               where getSpot = ((t !! y) !! x)

{- Calculate Manhattan distance between two vertices -}
manhDist (x,y) (cx,cy) = (x-cx)*(x-cx) + (y-cy)*(y-cy)

{- Comparing function for use with sortBy on tuples of (Vertex, path, and
   Manhattan distance to other bot -}
mDistSort (c,cps,d1) (b,bps,d2) | d1 < d2 = LT
                                | otherwise = GT






{-
  Series of functions to decide which way to move when cut off from the other
  bot.  First, sorts possible moves by flood amount.  Then out of those moves
  with the greatest flood values, looks for the move that is next to the most
  walls.
-}
step3Decide:: [[Spot]] -> [Move]
step3Decide tM | length children == 0 = []
                    | length children == 1 = [ locDiff (me tM) (head children) ]
                    | otherwise            = [ locDiff (me tM) sortW ]
     where children = adjBVert tM (me tM)
           sortW | length sF > 1 = sortByWalls tM sF
                 | otherwise     = head sF
                 where sF = sortByFlood3 tM children

sortByFlood3 :: [[Spot]] -> [Vertex] -> [Vertex]
sortByFlood3 tM [] = []
sortByFlood3 tM cs = filter (\x -> floodFill tM x [] == (maxFloodLength)) cs 
           where maxFloodLength = maximum [ floodFill tM x [] | x <- cs ]

sortByWalls :: [[Spot]] -> [Vertex] -> Vertex
sortByWalls tM (x:y:xs) = sortByWalls tM ((ff x y):xs)
   where ff a b | wallCount tM a > wallCount tM b = a
                | otherwise = b
sortByWalls tM (x:xs) = x

{- Returns Int value count of number of walls in the 8 cardinal directions. -}
wallCount tM (x,y) = sum [ 1 | a <- eightP, isWall tM a]
   where eightP = [(x+1,y+1),(x+1,y),(x+1,y-1),(x,y-1),(x-1,y-1),(x-1,y),(x-1,y+1),(x,y+1)]
         isWall t v = Wall == getSpot v
             where getSpot (x,y) = ((t !! y) !! x)





{- Series of functions to sort adjacent vertices when next to the other bot. -}
nextToDecide:: [[Spot]] -> [Move]
nextToDecide tM | length children == 0 = [locDiff (me tM) (them tM)]
                | length children == 1 = [ locDiff (me tM) (head children) ]
                | otherwise = [ locDiff (me tM) (nextToSort tM children) ]
     where children = adjBVert tM (me tM)


nextToSort :: [[Spot]] -> [Vertex] -> Vertex
nextToSort tM (x:y:xs) = nextToSort tM ((ff):xs)
   where ff | floodFill tM x (y:xs) == floodFill tM y (x:xs) = closestToThem tM x y
            | floodFill tM x (y:xs) > floodFill tM y (x:xs) = x
            | otherwise = y 
nextToSort tM (x:xs)   = x

closestToThem:: [[Spot]] -> Vertex -> Vertex -> Vertex
closestToThem tM a b | manhDist a (them tM) > manhDist b (them tM) = b
                     | otherwise = a




{- Functions to sort adjacent vertices when bots have one space between them. -}
minimaxDecide:: [[Spot]] -> [Move]
minimaxDecide tM | length children == 0 = []
                 | length children == 1 = [ locDiff (me tM) (head children) ]
                 | otherwise = [ locDiff (me tM) (minimax tM children) ]
     where children = adjBVert tM (me tM)


minimax:: [[Spot]] -> [Vertex] -> Vertex
minimax tM cs = minimaxSort tM (fList tM cs themV) (fList tM themV cs)
     where themV = adjBVert tM (them tM)

fList tM (x:xs) other = (x,(floodFill tM x (xs ++ other))):(fList tM xs other)
fList tM [] _ = []

minimaxSort tM (c1:c2:cs) ts
       | (minimaxValue c1 ts) > (minimaxValue c2 ts) = minimaxSort tM (c1:cs) ts
       | otherwise = minimaxSort tM (c2:cs) ts
           where minimaxValue c ts = sum [minimaxCompare tM c t | t <- ts]
                 minimaxCompare tM (c,cf) (t,tf)
                        | c == t = 0         --Tie position
                        | cf == 0 = (-100)   --Loss position
                        | tf == 0 = 100      --Win!
                        | otherwise = (cf - tf)
--                        | otherwise = (cf - tf) + (minimax2 tM c t (adjBVert tM c) )
minimaxSort tM (c:cs) ts = fst c

minimax2:: [[Spot]] -> Vertex -> Vertex -> [Vertex] -> Int
minimax2 tM cP tP cs = minimaxSort2 tM (fList tM cs (cP:tP:themV)) (fList tM themV (cP:tP:cs))
     where themV = adjBVert tM tP

minimaxSort2 tM (c1:cs) ts = (minimaxValue c1 ts) + minimaxSort2 tM (cs) ts
           where minimaxValue c ts = sum [minimaxCompare tM c t | t <- ts]
                 minimaxCompare tM (c,cf) (t,tf)
                        | c == t = 0         --Tie position
                        | cf == 0 = (-100)   --Loss position
                        | tf == 0 = 100      --Win!
                        | otherwise = (cf - tf)
minimaxSort2 tM [] _ = 0






{- 
  Series of functions to sort adjacent vertices by number of flood spaces.
  Used when player bot is within 6 moves of the other bot, so as to determine
  which move leads to the greatest open area while still generally moving
  towards the other bot to cut them off.
-}
floodFillDecide:: [[Spot]] -> [Move]
floodFillDecide tM | length children == 0 = []
                   | length children == 1 = [ locDiff (me tM) (head children) ]
                   | otherwise = [ locDiff (me tM) (sortByFlood tM children) ]
     where children = adjBVert tM (me tM)

sortByFlood :: [[Spot]] -> [Vertex] -> Vertex
sortByFlood tM (x:y:xs) = sortByFlood tM ((ff):xs)
   where ff | floodFill tM x (themV) == floodFill tM y (themV) = closestToThem tM x y
            | floodFill tM x (themV) > floodFill tM y (themV) = x
            | otherwise = y 
                where themV = adjBVert tM (them tM)
sortByFlood tM (x:xs)   = x


floodFill :: [[Spot]] -> Vertex -> [Vertex] -> Int 
floodFill tronMap source initVisited = help [source] initVisited
  where help [] visited = ((length $ nub visited) - (length initVisited))
        help (p:pending) visited 
              | ((length visited) - (length initVisited)) >= 125 = 125
              | otherwise = help (add children pending) $ nub (visited ++ children) 
            where children = adjBVert tronMap p
                  add [] qs = qs
                  add (c:cs) qs =
                     if elem c visited
                       then add cs qs
                       else add cs (c:qs)

{- Returns a list of the Blank vertices that adjacent to the passed vertex. -}
adjBVert:: [[Spot]] -> Vertex -> [Vertex]
adjBVert t (x,y) = [ a | a <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)], isBlank t a]
     where isBlank t v = Blank == getSpot v
             where getSpot (x,y) = ((t !! y) !! x)

-------------------- TESTING --------------------------

tm1string = [
   "5 5",
   "#####", 
   "#   #", 
   "###1#", 
   "#2 ##", 
   "#####"
   ] 
tm1 = map (map readSpot) (take 5 (tail tm1string))


tm2string = [
   "7 7",
   "#######", 
   "#  #  #", 
   "#  #  #", 
   "##2#  #", 
   "#  1 ##", 
   "#     #", 
   "#######"
   ] 
tm2 = map (map readSpot) (take 7 (tail tm2string))


tm5string = [
   "15 15" ,
   "###############" ,
   "############  #" ,
   "#          1 2#" ,
   "#            ##" ,
   "#            ##" ,
   "#            ##" ,
   "#            ##" ,
   "#            ##" ,
   "#            ##" ,
   "#            ##" ,
   "#            ##" ,
   "#            ##" ,
   "#            ##" ,
   "#            ##" ,
   "###############"
   ]
tm5 = map (map readSpot) (take 15 (tail tm5string))


tm6string = [
   "15 15" ,
   "###############" ,
   "##########    #" ,
   "#        #    #" ,
   "#        #    #" ,
   "#        #    #" ,
   "#        #    #" ,
   "#        #  ###" ,
   "#        #  ###" ,
   "#        #  ###" ,
   "#        #  ###" ,
   "#        1  ###" ,
   "#         2 ###" ,
   "#         #####" ,
   "#            ##" ,
   "###############"
   ]

tm6 = map (map readSpot) (take 15 (tail tm6string))

tm6Find = aStar tm6 (me tm6) (them tm6)


tm7string = [
   "15 15" ,
   "###############" ,
   "#      #      #" ,
   "#      #      #" ,
   "#  ###        #" ,
   "#    # #      #" ,
   "# #### #      #" ,
   "# #2   #      #" ,
   "### ####### ###" ,
   "#      ####   #" ,
   "#      ## #   #" ,
   "#      ## #   #" ,
   "#       1 ##  #" ,
   "#      #      #" ,
   "#      #      #" ,
   "###############"
   ]

tm7 = map (map readSpot) (take 15 (tail tm7string))
tm7Find = aStar tm7 (me tm7) (them tm7)

tm8string = [
   "15 15" ,
   "###############" ,
   "####          #" ,
   "#  #          #" ,
   "#  ######     #" ,
   "#       #     #" ,
   "#       2     #" ,
   "#        1#   #" ,
   "#         #   #" ,
   "#         #   #" ,
   "#         #   #" ,
   "#         #   #" ,
   "#         #   #" ,
   "#         ### #" ,
   "#           ###" ,
   "###############"
   ]
tm8 = map (map readSpot) (take 15 (tail tm8string))

------------------------------- {- Retired thoughts -} ----------------------

--import qualified Data.Map as M

--List of vertices, not including the outside walls
--vertexList:: [[Spot]] -> [Vertex]
--vertexList tronMap = [(x,y) | x <- [1..(length tronMap-2)], y <- [1..(length tronMap-2)] ]

--adjList tronMap = makeAdjList tronMap (vertexList tronMap)

--makeAdjList :: [[Spot]] -> [Vertex] -> M.Map Vertex [Vertex]
--makeAdjList t [] = M.empty
--makeAdjList t (x:xs) = M.insert x (adjVertices t x) $ makeAdjList t xs 

{- Simple function to remove "Just" from Maybe list results. -}
--justR (Just a) = a
--justR Nothing = []


{-
bfs :: [[Spot]] -> Vertex -> Vertex -> [Vertex]
bfs tronMap source dest = help [(source,[source])] []
  where help [] _ = []
        help ((p,ps):pending) visited 
              | p == dest = ps
              | otherwise = help (add children pending) (p:visited ++ children)
            where children = adjOpenVert tronMap p
                  add [] qs = qs
                  add (c:cs) qs =
                     if elem c visited
                       then add cs qs
                       else add cs (qs ++ [(c,c:ps)])
-}

{- Find the longest path out of Player position  -}
{-
longPath :: [[Spot]] -> [Move]
longPath tM | length p == 1 = [ locDiff (me tM) (head ((last . init) p)) ]
            | length p > 1  = [ locDiff (me tM) (sortByWalls tM (nub(map (last . init) p))) ]
            | otherwise     = []
   where p = maxed ( paths tM (me tM) )
           where maxed xs = (filter (\x -> length x >= (maximum $ map length xs)) xs)

paths :: [[Spot]] -> Vertex -> [[Vertex]]
paths tronMap source = help [(source,[source],[source])]
  where help [] = []
        help ((p,ps,vs):pending) = (p:ps) : help (add children pending)
            where children = adjBVert tronMap p
                  add [] qs = qs
                  add (c:cs) qs =
                     if elem c ps
                       then add cs qs
                       else if length ps < 15
                          then add cs (qs ++ [(c,c:ps)])
                          else add cs qs
-}
{-
sortAwayWalls :: [[Spot]] -> [Vertex] -> [Vertex]
sortAwayWalls tM (x:y:xs) 
        | wallCount tM x > wallCount tM y = y : sortAwayWalls tM (x:xs)
        | otherwise                       = x : sortAwayWalls tM (y:xs)
sortAwayWalls tM (x:xs) = [x]
sortAwayWalls tM [] = []
-}

