import Data.List

type Triangle = (Integer, Integer, Integer)

type Row = (Integer, Integer, Integer)
type Matrix = (Row, Row, Row)

mU = ((1, -2, 2),
      (2, -1, 2),
      (2, -2, 3))

{-
P((a, b, c) * mU) = 5a - 5b + 7c
5a - 5b + 7c ? a + b + c
4a - 6b + 6c ? 0
6c > 6b => P(t * U) > P(t)
-}

mA = ((1, 2, 2),
      (2, 1, 2),
      (2, 2, 3))
     
-- seems that P(t * A) > P(t)

mD = ((-1, 2, 2),
      (-2, 1, 2),
      (-2, 2, 3))
     
-- P(t * D) = 4a + b + 3c > P(t)

     
(*|) :: Triangle -> Row -> Integer
(a, b, c) *| (ai, bi, ci) = a*ai + b*bi + c*ci
     
(|*|) :: Triangle -> Matrix -> Triangle
t |*| (r0, r1, r2) = let a' = t *| r0
                         b' = t *| r1
                         c' = t *| r2
                     in (a', b', c')
                        

maxP = 1500000

matrices = [mU, mA, mD]

enumerateTriangles :: [Triangle]
enumerateTriangles = enumerateTrianglesSince (3, 4, 5)

enumerateTrianglesSince :: Triangle -> [Triangle]
enumerateTrianglesSince t = let ts = map (t |*|) matrices
                                trs = t : concatMap enumerateTrianglesSince ts
                            in takeWhile ((<= maxP) . perimeter) trs

perimeter (a, b, c) = a + b + c


enumeratePerimeters :: [Integer]
enumeratePerimeters = map perimeter enumerateTriangles

enumerateAllPerimeters = concatMap nonPrimitives enumeratePerimeters

nonPrimitives n = takeWhile (<= maxP) (map (*n) [1..])

solution = length $ filter ((==1) . length) $ group $ sort enumerateAllPerimeters