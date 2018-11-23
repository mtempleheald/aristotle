import Data.List
-- Problem: Hexagonal grid containing numbers 1-19.  
-- Each straight line should sum to 38, what are the possible solutions?
------------------
--     a1  a2  a3
--   b1  b2  b3  b4
-- c1  c2  c3  c4  c5
--   d1  d2  d3  d4
--     e1  e2  e3
------------------
main = do
       let results= [((a1,a2,a3),(b1,b2,b3,b4),(c1,c2,c3,c4,c5),(d1,d2,d3,d4),(e1,e2,e3))
                    |a1 <- [1..19]
                    ,a2 <- [1..19]\\[a1]
                    ,a3 <- [1..19]\\[a1,a2]
                    ,a1 + a2 + a3 == 38
                    ,b1 <- [1..19]\\[a1,a2,a3]
                    ,c1 <- [1..19]\\[a1,a2,a3,b1]
                    ,a1 + b1 + c1 == 38
                    ,d1 <- [1..19]\\[a1,a2,a3,b1,         c1]
                    ,e1 <- [1..19]\\[a1,a2,a3,b1,         c1,            d1]
                    ,c1 + d1 + e1 == 38
                    ,e2 <- [1..19]\\[a1,a2,a3,b1,         c1,            d1,         e1]
                    ,e3 <- [1..19]\\[a1,a2,a3,b1,         c1,            d1,         e1,e2]
                    ,e1 + e2 + e3 == 38
                    ,d4 <- [1..19]\\[a1,a2,a3,b1,         c1,            d1,         e1,e2,e3]
                    ,c5 <- [1..19]\\[a1,a2,a3,b1,         c1,            d1,      d4,e1,e2,e3]
                    ,e3 + d4 + c5 == 38
                    ,b4 <- [1..19]\\[a1,a2,a3,b1,         c1,         c5,d1,      d4,e1,e2,e3]
                    ,a3 + b4 + c5 == 38
                    ,b2 <- [1..19]\\[a1,a2,a3,b1,      b4,c1,         c5,d1,      d4,e1,e2,e3]
                    ,b3 <- [1..19]\\[a1,a2,a3,b1,b2,   b4,c1,         c5,d1,      d4,e1,e2,e3]
                    ,b1 + b2 + b3 + b4 == 38
                    ,c2 <- [1..19]\\[a1,a2,a3,b1,b2,b3,b4,c1,         c5,d1,      d4,e1,e2,e3]
                    ,a2 + b2 + c2 + d1 == 38
                    ,d2 <- [1..19]\\[a1,a2,a3,b1,b2,b3,b4,c1,c2,      c5,d1,      d4,e1,e2,e3]
                    ,b1 + c2 + d2 + e2 == 38
                    ,d3 <- [1..19]\\[a1,a2,a3,b1,b2,b3,b4,c1,c2,      c5,d1,d2,   d4,e1,e2,e3]
                    ,d1 + d2 + d3 + d4 == 38
                    ,c4 <- [1..19]\\[a1,a2,a3,b1,b2,b3,b4,c1,c2,      c5,d1,d2,d3,d4,e1,e2,e3]
                    ,e2 + d3 + c4 + b4 == 38
                    ,c3 <- [1..19]\\[a1,a2,a3,b1,b2,b3,b4,c1,c2,   c4,c5,d1,d2,d3,d4,e1,e2,e3]
                    ,c1 + c2 + c3 + c4 + c5 == 38
                    ,a1 + b2 + c3 + d3 + e3 == 38
                    ,e1 + d2 + c3 + b3 + a3 == 38
                    ]
       putStrLn $ show results
       let 
       putStrLn $ show $ rotate60degrees $ head results

-- Provides 12 possible solutions:
-- [((3,17,18),(19,7,1,11),(16,2,5,6,9),(12,4,8,14),(10,13,15)),
--  ((3,19,16),(17,7,2,12),(18,1,5,4,10),(11,6,8,13),(9,14,15)),
--  ((9,11,18),(14,6,1,17),(15,8,5,7,3),(13,4,2,19),(10,12,16)),
--  ((9,14,15),(11,6,8,13),(18,1,5,4,10),(17,7,2,12),(3,19,16)),
--  ((10,12,16),(13,4,2,19),(15,8,5,7,3),(14,6,1,17),(9,11,18)),
--  ((10,13,15),(12,4,8,14),(16,2,5,6,9),(19,7,1,11),(3,17,18)),
--  ((15,13,10),(14,8,4,12),(9,6,5,2,16),(11,1,7,19),(18,17,3)),
--  ((15,14,9),(13,8,6,11),(10,4,5,1,18),(12,2,7,17),(16,19,3)),
--  ((16,12,10),(19,2,4,13),(3,7,5,8,15),(17,1,6,14),(18,11,9)),
--  ((16,19,3),(12,2,7,17),(10,4,5,1,18),(13,8,6,11),(15,14,9)),
--  ((18,11,9),(17,1,6,14),(3,7,5,8,15),(19,2,4,13),(16,12,10)),
--  ((18,17,3),(11,1,7,19),(9,6,5,2,16),(14,8,4,12),(15,13,10))]

-- a quick glance shows that these are actually the same solution, 6 rotations, 2 mirrors
-- this is demonstrably using 2 functions - rotate60degrees and mirror
-- I'm sure there's an elegant way to implement these, but the obvious way to me is:
type Grid = ((Int,Int,Int),(Int,Int,Int,Int),(Int,Int,Int,Int,Int),(Int,Int,Int,Int),(Int,Int,Int))
rotate60 :: Grid -> Grid
rotate60 (  (a1, a2, a3),
          (b1, b2, b3, b4),
        (c1, c2, c3, c4, c5),
          (d1, d2, d3, d4),
            (e1, e2, e3)    )
    =    ( (c1, b1, a1),
          (d1, c2, b2, a2),
        (e1, d2, c3, b3, a3),
          (e2, d3, c4, b4),
            (e3, d4, c5)    )
mirror :: Grid -> Grid
mirror  (   (a1, a2, a3),
          (b1, b2, b3, b4),
        (c1, c2, c3, c4, c5),
          (d1, d2, d3, d4),
            (e1, e2, e3)    )
    =   (   (a3 , a2, a1),
          (b4, b3, b2, b1),
        (c5, c4, c3, c2, c1),
          (d4 , d3, d2, d1),
            (e3 , e2, e1))
-- to test these would be very easy (QuickCheck):
-- test == rotate60 $ rotate60 $ rotate60 $ rotate60 $ rotate60 $ rotate60 test
-- test == mirror $ mirror test
-- not going to bother for now though