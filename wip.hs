--{-# LANGUAGE ParallelListComp #-}
{-module Hexapuzzle (
    main, generatePerms, generateHexgrid, binaryGridX
    )
where-}
-- -------------------------------------------------------------------------
-- Problem: Hexagonal grid containing numbers 1-19.  
-- Each straight line should sum to 38, what are the possible solutions?
--     1   2   3
--   4   5   6   7
-- 8   9  10  11  12
--  13  14  15  16
--    17  18  19
-- -------------------------------------------------------------------------
-- Solution:
-- Create "hexgrid" datatype to hold structure e.g. [[1,2,3],[4,5,6,7],[8,9,10,11,12],[13,14,15,16],[17,18,19]]
-- Function generatePerms: 
--   Generate all permutations of numbers 1-19 in an array of flat arrays (121,645,100,408,832,000 possible permutations)
--   Return this, transformed to [hexgrid]
-- Function removeInvalids:
--   Accept [hexgrid] return smaller [hexgrid] containing only correct permutations
--   Do this by folding sum by row (each 38), expect 38*5 = 190
-- Function rotate60:
--   Accept [hexgrid], return [hexgrid] containing same data, rotated 60 degrees
--   Do this by... https://www.haskell.org/tutorial/arrays.html#sect13.5
-- Function main:
--   Use hardcoded 19 parameter to generatePerms
--   Then removeInvalids, rotate60, removeInvalids, rotate60, removeInvalids, print
-- -------------------------------------------------------------------------
--import Control.Monad.Writer
import Control.Parallel  (par, pseq)
import Data.List (permutations, unzip6, intersect, (\\))
import GHC.Conc (numCapabilities)

-- solve using simultaneous solutions
generateSolns :: Int -> Int -> [[Int]]
generateSolns x t = [[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s]
                   |a <- [1..x]
                   ,b <- [1..x], b /= a
                   ,c <- [1..x], c /= a, c /= b
                   ,d <- [1..x], d /= a, d /= b, d /= c
                   ,e <- [1..x], e /= a, e /= b, e /= c, e /= d
                   ,f <- [1..x], f /= a, f /= b, f /= c, f /= d, f /= e
                   ,g <- [1..x], g /= a, g /= b, g /= c, g /= d, g /= e, g /= f
                   ,h <- [1..x], h /= a, h /= b, h /= c, h /= d, h /= e, h /= f, h /= g
                   ,i <- [1..x], i /= a, i /= b, i /= c, i /= d, i /= e, i /= f, i /= g, i /= h
                   ,j <- [1..x], j /= a, j /= b, j /= c, j /= d, j /= e, j /= f, j /= g, j /= h, j /= i
                   ,k <- [1..x], k /= a, k /= b, k /= c, k /= d, k /= e, k /= f, k /= g, k /= h, k /= i, k /= j
                   ,l <- [1..x], l /= a, l /= b, l /= c, l /= d, l /= e, l /= f, l /= g, l /= h, l /= i, l /= j, l /= k
                   ,m <- [1..x], m /= a, m /= b, m /= c, m /= d, m /= e, m /= f, m /= g, m /= h, m /= i, m /= j, m /= k, m /= l
                   ,n <- [1..x], n /= a, n /= b, n /= c, n /= d, n /= e, n /= f, n /= g, n /= h, n /= i, n /= j, n /= k, n /= l, n /= m
                   ,o <- [1..x], o /= a, o /= b, o /= c, o /= d, o /= e, o /= f, o /= g, o /= h, o /= i, o /= j, o /= k, o /= l, o /= m, o /= n
                   ,p <- [1..x], p /= a, p /= b, p /= c, p /= d, p /= e, p /= f, p /= g, p /= h, p /= i, p /= j, p /= k, p /= l, p /= m, p /= n, p /= o
                   ,q <- [1..x], q /= a, q /= b, q /= c, q /= d, q /= e, q /= f, q /= g, q /= h, q /= i, q /= j, q /= k, q /= l, q /= m, q /= n, q /= o, q /= p
                   ,r <- [1..x], r /= a, r /= b, r /= c, r /= d, r /= e, r /= f, r /= g, r /= h, r /= i, r /= j, r /= k, r /= l, r /= m, r /= n, r /= o, r /= p, r /= q
                   ,s <- [1..x], s /= a, s /= b, s /= c, s /= d, s /= e, s /= f, s /= g, s /= h, s /= i, s /= j, s /= k, s /= l, s /= m, s /= n, s /= o, s /= p, s /= q, s /= r
                   ,a + b + c         == t, c + g + l         == t, l + p + s         == t
                   ,d + e + f + g     == t, b + f + k + p     == t, g + k + o + r     == t
                   ,h + i + j + k + l == t, a + e + j + o + s == t, c + f + j + n + q == t
                   ,m + n + o + p     == t, d + i + n + r     == t, b + e + i + m     == t
                   ,q + r + s         == t, h + m + q         == t, a + d + h         == t
                    {-a `notElem` [b..s]-}
                   ]

-- generate array of flat element arrays, all possible permutations
-- this creates 19! permutations, probably better to concatenate the results of the solutions to 1 of the 3 dimensions, then filter these futher
{-
generatePerms :: Int -> [[Int]]
generatePerms x = permutations [1..x]
-}
generatePerms :: [[Int]]
generatePerms = gp3 . gp4 . gp5 . gp4 $ threePerms [1..19] 38
-- generate permutations of 3, adding to 38
-- for each of these work out permutations of 4 adding to 38 out of integers left over, create list of 7-length lists
-- add on 5s, 4s and finally 3s with what's left
gp3 :: [[Int]] -> [[Int]]
gp3 [] = []
gp3 (x:xs) = map ((++) x) (threePerms ([1..19]\\x) 38) ++ gp3 xs
gp4 :: [[Int]] -> [[Int]]
gp4 [] = []
gp4 (x:xs) = map ((++) x) (fourPerms  ([1..19]\\x) 38) ++ gp4 xs
gp5 :: [[Int]] -> [[Int]]
gp5 [] = []
gp5 (x:xs) = map ((++) x) (fivePerms  ([1..19]\\x) 38) ++ gp5 xs

threePerms :: [Int] -> Int -> [[Int]]
threePerms values target = [[a,b,c]| a <- values, b <- values, c <- values
                                   , a + b + c == target
                                   , a /= b, a /= c, b /= c]
fourPerms :: [Int] -> Int -> [[Int]]
fourPerms values target = [[a,b,c,d]| a <- values, b <- values, c <- values, d <- values
                                   , a + b + c + d == target
                                   , a /= b, a /= c, a /= d, b /= c, b /= d, c /= d]          
fivePerms :: [Int] -> Int -> [[Int]]
fivePerms values target = [[a,b,c,d,e]| a <- values, b <- values, c <- values, d <- values, e <- values
                                   , a + b + c + d + e == target
                                   , a /= b, a /= c, a /= d, a /= e, b /= c, b /= d, b /= e, c /= d, c /= e, d /= e]    

-- Generate all valid permutations of 3 tiles to form an edge containing 3 tiles
type Edge = (Int,Int,Int)
threes :: [Int] -> Int -> [Edge]
threes values target = [(a,b,c)| a <- values, b <- values, c <- values
                               , a + b + c == target
                               , a /= b, a /= c, b /= c]
frst3 :: Edge -> Int
frst3 (x,_,_) = x
scnd3 :: Edge -> Int
scnd3 (_,x,_) = x
thrd3 :: Edge -> Int
thrd3 (_,_,x) = x

frst6 :: Perimeter -> Edge
frst6 (x,_,_,_,_,_) = x
scnd6 :: Perimeter -> Edge
scnd6 (_,x,_,_,_,_) = x
thrd6 :: Perimeter -> Edge
thrd6 (_,_,x,_,_,_) = x
frth6 :: Perimeter -> Edge
frth6 (_,_,_,x,_,_) = x
ffth6 :: Perimeter -> Edge
ffth6 (_,_,_,_,x,_) = x
sxth6 :: Perimeter -> Edge
sxth6 (_,_,_,_,_,x) = x

shared :: Edge -> Edge -> Int
shared (a1,a2,a3) (b1,b2,b3) = (if (a1 == b1 || a1 == b2 || a1 == b3) then 1 else 0)
                             + (if (a2 == b1 || a2 == b2 || a2 == b3) then 1 else 0)
                             + (if (a3 == b1 || a3 == b2 || a3 == b3) then 1 else 0)

-- Given all valid permutations of 3 we know all possible edges, chain 6 together to form the perimeter of the hexagon
type Perimeter = (Edge,Edge,Edge,Edge,Edge,Edge)
perimeters :: [Edge] -> [Perimeter]
perimeters edges = perimeters' (
                     perimeters' (
                       perimeters' (
                         perimeters' (
                           perimeters' (
                             perimeters' [] edges 1
                           ) edges 2
                         ) edges 3
                       ) edges 4
                     ) edges 5
                   ) edges 6
-- Consider the perimeter as a ring of edges, a3==b1, b2==c1...f3==a1
--     a1 a2 b1
--   f2        b2
-- f1            c1
--   e2        c2
--     e1 d2 d1
perimeters' :: [Perimeter] -> [Edge] -> Int -> [Perimeter]
perimeters' p es d
            | d == 1    = [(a,(0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0)) | a <- es] -- set up all possible first edge permutations
            | d == 2    = [((a1,a2,a3),(b1,b2,b3),(0,0,0),(0,0,0),(0,0,0),(0,0,0)) 
                          | (a1,a2,a3) <- map (frst6) p
                          , (b1,b2,b3) <- es
                          , b1 == a3                         -- chain 2nd edge to 1st
                          , shared (a1,a2,a3) (b1,b2,b3) == 1-- remove other duplicates
                          ]
            | d == 3    = [((a1,a2,a3),(b1,b2,b3),(c1,c2,c3),(0,0,0),(0,0,0),(0,0,0)) 
                          | ((a1,a2,a3),(b1,b2,b3),(0,0,0),(0,0,0),(0,0,0),(0,0,0)) <- p
                          , (c1,c2,c3) <- es
                          , c1 == b3 -- chain 3rd edge to 2nd
                          , shared (a1,a2,a3) (c1,c2,c3) == 0
                          , shared (b1,b2,b3) (c1,c2,c3) == 1
                          ]
            | d == 4    = [((a1,a2,a3),(b1,b2,b3),(c1,c2,c3),(d1,d2,d3),(0,0,0),(0,0,0)) 
                          | ((a1,a2,a3),(b1,b2,b3),(c1,c2,c3),(0,0,0),(0,0,0),(0,0,0)) <- p
                          , (d1,d2,d3) <- es
                          , d1 == c3 -- chain 4th edge to 3rd
                          , shared (a1,a2,a3) (d1,d2,d3) == 0
                          , shared (b1,b2,b3) (d1,d2,d3) == 0
                          , shared (c1,c2,c3) (d1,d2,d3) == 1
                          ]
            | d == 5    = [((a1,a2,a3),(b1,b2,b3),(c1,c2,c3),(d1,d2,d3),(e1,e2,e3),(0,0,0)) 
                          | ((a1,a2,a3),(b1,b2,b3),(c1,c2,c3),(d1,d2,d3),(0,0,0),(0,0,0)) <- p
                          , (e1,e2,e3) <- es
                          , e1 == d3 -- chain 5th edge to 4th
                          , shared (a1,a2,a3) (e1,e2,e3) == 0
                          , shared (b1,b2,b3) (e1,e2,e3) == 0
                          , shared (c1,c2,c3) (e1,e2,e3) == 0
                          , shared (d1,d2,d3) (e1,e2,e3) == 1
                          ]
            | d == 6    = [((a1,a2,a3),(b1,b2,b3),(c1,c2,c3),(d1,d2,d3),(e1,e2,e3),(f1,f2,f3)) 
                          | ((a1,a2,a3),(b1,b2,b3),(c1,c2,c3),(d1,d2,d3),(e1,e2,e3),(0,0,0)) <- p
                          , (f1,f2,f3) <- es
                          , f1 == e3 -- chain 6th edge to 5th
                          , f3 == a1 -- chain 6th edge to 1st
                          , shared (a1,a2,a3) (f1,f2,f3) == 1
                          , shared (b1,b2,b3) (f1,f2,f3) == 0
                          , shared (c1,c2,c3) (f1,f2,f3) == 0
                          , shared (d1,d2,d3) (f1,f2,f3) == 0
                          , shared (e1,e2,e3) (f1,f2,f3) == 1
                          ]
            | otherwise = []

type FullGrid = ((Int,Int,Int),(Int,Int,Int,Int),(Int,Int,Int,Int,Int),(Int,Int,Int,Int),(Int,Int,Int))
ySolutions :: [Perimeter] -> [FullGrid]
ySolutions p = [((a1,a2,a3),(b1,b2,b3,b4),(c1,c2,c3,c4,c5),(d1,d2,d3,d4),(e1,e2,e3))
               |((a1,a2,a3),(_,b4,_),(c5,d4,_),(e3,e2,e1),(_,d1,c1),(_,b1,_)) <- p -- map perimeter structure to full structure
               ,b2 <- [1..19]\\[a1,a2,a3,b1,      b4,c1,         c5,d1,      d4,e1,e2,e3]
               ,b3 <- [1..19]\\[a1,a2,a3,b1,b2,   b4,c1,         c5,d1,      d4,e1,e2,e3]
               ,b1 + b2 + b3 + b4 == 38
               ,c2 <- [1..19]\\[a1,a2,a3,b1,b2,b3,b4,c1,         c5,d1,      d4,e1,e2,e3]
               ,a2 + b2 + c2 + d1 == 38
               ,c4 <- [1..19]\\[a1,a2,a3,b1,b2,b3,b4,c1,c2,      c5,d1,      d4,e1,e2,e3]
               ,a2 + b3 + c4 + d4 == 38
               ,d2 <- [1..19]\\[a1,a2,a3,b1,b2,b3,b4,c1,c2,   c4,c5,d1,      d4,e1,e2,e3]
               ,b1 + c2 + d2 + e2 == 38
               ,d3 <- [1..19]\\[a1,a2,a3,b1,b2,b3,b4,c1,c2,   c4,c5,d1,d2,   d4,e1,e2,e3]
               ,d1 + d2 + d3 + d4 == 38
               ,b4 + c4 + d3 + e2 == 38
               ,c3 <- [1..19]\\[a1,a2,a3,b1,b2,b3,b4,c1,c2,   c4,c5,d1,d2,d3,d4,e1,e2,e3]            
               ,c1 + c2 + c3 + c4 + c5 == 38
               ,a3 + b3 + c3 + d2 + e1 == 38
               ,a1 + b2 + c3 + d3 + e3 == 38
               ]

{-perimeters' edges depth = [(a,b,c,d,e,f)| a <- edges, b <- edges, c <- edges, d <- edges, e <- edges, f <- edges
                          , thd3 a == fst b
                          , thd3 b == fst c
                          , thd3 c == fst d
                          , thd3 d == fst e
                          , thd3 e == fst f
                          , thd3 f == fst a]-}

{- generate a hexagonal grid with side length (or concentric circle count) matching the given number
   use axial co-ordinates to define each grid location, e.g.:
            [        ( 0,-2),( 1,-2),( 2,-2)
            ,    (-1,-1),( 0,-1),( 1,-1),( 2,-1)
            ,(-2, 0),(-1, 0),( 0, 0),( 1,0 ),( 2, 0)
            ,    (-2, 1),(-1, 1),( 0, 1),( 1, 1)
            ,        (-2, 2),(-1, 2),( 0, 2)]
-}
type Hexgrid = [(Int,Int)]
generateHexgrid :: Int -> Hexgrid
generateHexgrid h = [(x,y) | y <- [1-h..h-1], x <- [1-h..h-1], abs (x + y) < h]

-- define a data type with same length as our permutation 
-- containing only 0 and 1 for a given co-ordinate direction/value combination
-- example above: binaryGridY example (-2) = [1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
-- when zipWithed and summed with [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19] we get 1+2+3=6
binaryGridX :: Hexgrid -> Int -> [Int]
binaryGridX h n = map (\x -> if fst x == n then 1 else 0) h
binaryGridY :: Hexgrid -> Int -> [Int]
binaryGridY h n = map (\x -> if snd x == n then 1 else 0) h
binaryGridZ :: Hexgrid -> Int -> [Int]
binaryGridZ h n = map (\x -> if ((fst x + snd x) == (-n)) then 1 else 0) h

-- define a data type to hold the zip between the game grid and the permutation
type GameTile = ((Int,Int),Int)
type GameGrid = [GameTile]
-- define functions which allow us to get at the individual values within the nested tuple
tileValue :: GameTile -> Int
tileValue ((_,_),v) = v
tileX :: GameTile -> Int
tileX ((x,_),_) = x
tileY :: GameTile -> Int
tileY ((_,y),_) = y
tileZ :: GameTile -> Int
tileZ ((x,y),_) = -x -y

-- remove invalid permutations from the complete list
-- map over the list of lists, apply validation logic to each sublist (permutation)
-- validatePermutation will return empty string if invalid, so filter these out
{-removeInvalids :: Hexgrid -> [[Int]] -> Writer [String] [[Int]]
removeInvalids grid perms = do
               tell [show $ head perms]
               return (filter (not . null) (map validatePermutation perms))-}
removeInvalids :: Hexgrid -> [[Int]] -> [[Int]]
removeInvalids grid perms = filter (not . null) (map validatePermutation (take 100000000 perms))
               where target = (2 * (length grid))
                     xm2 = binaryGridX grid (-2)
                     --ym2 = binaryGridY grid (-2)
                     zm2 = binaryGridZ grid (-2)
                     x2  = binaryGridX grid 2
                     --y2  = binaryGridY grid 2
                     z2  = binaryGridZ grid 2
                     xm1 = binaryGridX grid (-1)
                     --ym1 = binaryGridY grid (-1)
                     zm1 = binaryGridZ grid (-1)
                     x1  = binaryGridX grid 1
                     --y1  = binaryGridY grid 1
                     z1  = binaryGridZ grid 1
                     x0  = binaryGridX grid 0
                     --y0  = binaryGridY grid 0
                     z0  = binaryGridZ grid 0
                     -- valid permutation => return itself, invalid permutation => return empty string
                     validatePermutation :: [Int] -> [Int]
                     validatePermutation p
                                         | sum (zipWith (*) xm2 p) /= target = []
                                        -- | sum (zipWith (*) ym2 p) /= target = []
                                         | sum (zipWith (*) zm2 p) /= target = []
                                         | sum (zipWith (*) x2  p) /= target = []
                                        -- | sum (zipWith (*) y2  p) /= target = []
                                         | sum (zipWith (*) z2  p) /= target = []
                                         | sum (zipWith (*) xm1 p) /= target = []
                                        -- | sum (zipWith (*) ym1 p) /= target = []
                                         | sum (zipWith (*) zm1 p) /= target = []
                                         | sum (zipWith (*) x1  p) /= target = []
                                        -- | sum (zipWith (*) y1  p) /= target = []
                                         | sum (zipWith (*) z1  p) /= target = []
                                         | sum (zipWith (*) x0  p) /= target = []
                                        -- | sum (zipWith (*) y0  p) /= target = []
                                         | sum (zipWith (*) z0  p) /= target = []
                                         | otherwise = p
                 
{- Abandon in favour of including as a function within removeInvalids
   Because these functions are closely coupled and can more easily add Writer to record progress
validatePermutation :: Hexgrid -> Int -> [Int] -> [Int] -> [Int] -> [Int]
validatePermutation _ _ _ _ [] = []
validatePermutation grid target x0 y0 perm
    | sum (zipWith (*) perm x0) /= target = []
    | sum (zipWith (*) perm y0) /= target = []
    | otherwise = perm
    where x0 = binaryGridX grid 0
          y0 = binaryGridY grid 0
          z0 = binaryGridZ grid 0
-}
{- Abandon in favour of precomputing binary array to improve efficiency (above)
validatePermutation grid target perm
    | sum (map (\i -> if (tileX i) == (0)  then tileValue i else 0) soln) /= target = []
    | sum (map (\i -> if (tileY i) == (0)  then tileValue i else 0) soln) /= target = []
    | sum (map (\i -> if (tileZ i) == (0)  then tileValue i else 0) soln) /= target = []
    | sum (map (\i -> if (tileX i) == (-1) then tileValue i else 0) soln) /= target && (length grid) > 1 = []
    | sum (map (\i -> if (tileY i) == (-1) then tileValue i else 0) soln) /= target && (length grid) > 1 = []
    | sum (map (\i -> if (tileZ i) == (-1) then tileValue i else 0) soln) /= target && (length grid) > 1 = []
    | sum (map (\i -> if (tileX i) == (1)  then tileValue i else 0) soln) /= target && (length grid) > 1 = []
    | sum (map (\i -> if (tileY i) == (1)  then tileValue i else 0) soln) /= target && (length grid) > 1 = []
    | sum (map (\i -> if (tileZ i) == (1)  then tileValue i else 0) soln) /= target && (length grid) > 1 = []
    | sum (map (\i -> if (tileX i) == (-2) then tileValue i else 0) soln) /= target && (length grid) > 2 = []
    | sum (map (\i -> if (tileY i) == (-2) then tileValue i else 0) soln) /= target && (length grid) > 2 = []
    | sum (map (\i -> if (tileZ i) == (-2) then tileValue i else 0) soln) /= target && (length grid) > 2 = []
    | sum (map (\i -> if (tileX i) == (2)  then tileValue i else 0) soln) /= target && (length grid) > 2 = []
    | sum (map (\i -> if (tileY i) == (2)  then tileValue i else 0) soln) /= target && (length grid) > 2 = []
    | sum (map (\i -> if (tileZ i) == (2)  then tileValue i else 0) soln) /= target && (length grid) > 2 = []
    | otherwise = perm
  where soln = zip grid perm
-}
{- Abandon in favour of neater and faster guard conditions (above)
    validatePermutation grid target perm = do
    let soln = (zip grid perm)
    -- hex is of the type GameGrid, i.e. [((x,y),value)]
    -- need to map-filter over tiles by groups and ensure that each sums to 38
    -- filter x = -2 and sum values, likewise x..z, -2..2
    if    sum (map (\t -> if (tileX t) ==(-1) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileX t) ==( 0) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileX t) ==( 1) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileX t) ==( 2) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileY t) ==(-2) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileY t) ==(-1) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileY t) ==( 0) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileY t) ==( 1) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileY t) ==( 2) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileZ t) ==(-2) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileZ t) ==(-1) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileZ t) ==( 0) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileZ t) ==( 1) then tileValue t else 0) soln) == 2 * target
       && sum (map (\t -> if (tileZ t) ==( 2) then tileValue t else 0) soln) == 2 * target
    then x
    else [] 
-}

main = do
--       let grid = generateHexgrid 3
--       let perms = generatePerms -- $ length grid
--       let perm = head perms
--       putStrLn $ "First permutation: " ++ show perm
       let es = threes [1..19] 38
       --putStrLn $ "Permutations of 3 from [1..19]: " ++ show (length es)

       --putStrLn $ show $ length $ perimeters es
       putStrLn $ show $ {-take 10 $-} ySolutions $ perimeters es
       --putStrLn $ show $ thrd3 (1,2,3)
       --putStrLn $ show $ ffth6 ((1,1,1),(2,2,2),(3,3,3),(4,4,4),(5,5,5),(6,6,6))

{-
       putStrLn $ "Zipped: " ++ show (zip grid perm)
       let x2 = binaryGridX grid (-2)
       putStrLn $ "X=2 bitmap: " ++ show x2
       let zipWithed = zipWith (*) x2 perm
       putStrLn $ "(*) applied using ZipWith: " ++ show zipWithed
       putStrLn $ "Sum of this example: " ++ show (sum zipWithed)
-}
--       putStrLn $ "number of cores: " ++ show numCapabilities 
--       putStrLn "Valid permutations: " 
--       putStrLn . show $ generateSolns 19 38
--       putStrLn . show . head $ removeInvalids grid perms
{-       let result = runWriter (removeInvalids grid perms)
       putStrLn . show . head . fst $ result
       mapM_ putStrLn . snd $ result
 -}      
       







--import Data.List.Split
--splitPlaces :: Integral a => [a] -> [e] -> [[e]] 

-- define a synonym for our data grid for a proposed solution
--type Hexgrid = [[Int]]

-- convert a given flat permutation array into our expected format
--convertToHexgrid :: [Int] -> Hexgrid
--convertToHexgrid x = splitPlaces [3,4,5,4,3] x

-- rotate our grid 60 degrees by flattening, recursively reordering then restructuring
--  1~3  2~7   3~12 
--  4~2  5~6   6~11  7~16 
--  8~1  9~5  10~10 11~15 12~9 
-- 13~4 14~9  15~14 16~18 
-- 17~8 18~13 19~17
--rotate60 :: Hexgrid -> Hexgrid
--rotate60 h = 

-- Cube co-ordinates for this bounded hexagonal structure
-- http://www.redblobgames.com/grids/hexagons/
{- (x, y, z) where x + y + z = 0
[                      ( 0, 2,-2),           ( 1, 1,-2),           ( 2, 0,-2)
,           (-1, 2,-1),           ( 0, 1,-1),           ( 1, 0,-1),           ( 2,-1,-1)
,(-2, 2, 0),           (-1, 1, 0),           ( 0, 0, 0),           ( 1,-1, 0),          ( 2,-2, 0)
,           (-2, 1, 1),           (-1, 0, 1),           ( 0,-1, 1),           ( 1,-2, 1)
,                      (-2, 0, 2),           (-1,-1, 2),           ( 0,-2, 2)]
-}

-- axial co-ordinate helper functions
{-getx :: (Int,Int) -> Int
getx (a,_) = a
gety :: (Int,Int) -> Int
gety (_,b) = b
getz :: (Int,Int) -> Int
getz (a,b) = 0 - a - b-}
