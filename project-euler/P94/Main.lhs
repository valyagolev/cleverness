
Okay, if A = x, B = x and C = x+1 are sides, then

h{C} = A * sin(alpha)

cos(alpha) = (C/2) / A


sin(alpha) = sqrt( 1 - cos(alpha) ^ 2 )

cos2(alpha) = (x+1)^2/4x^2

1 - cos2(alpha) = sin2(alpha)

S = h{C} * C = A * sin(alpha) * C / 2= x * (x + 1) * sqrt( 1 - cos2(alpha) ) / 2

let's try...

> module P94.Main where

> import Data.Ratio
> import Data.Maybe (maybeToList)

> import ProjectEuler (maybeSqrt)

> fromRatioInt :: Ratio Int -> Int
> fromRatioInt r | denominator r == 1 = numerator r 
> fromRatioInt _ = undefined

> getP :: Int -> Int -> Int -> Int
> getP x c z = let xr = x % 1
>                  cr = c % 1
>                  sinA = z % (2 * x) in fromRatioInt (xr * cr * sinA / 2)

> getZ2 :: Int -> Int -> Int
> getZ2 x c = 4 * x * x - c * c


4x^2 - c^2 should be a square


> solutions' :: [((Int, Int), Int)]
> solutions' = do x <- [3,5..]
>                 c <- [x - 1, x + 1]
>                 let z2 = getZ2 x c
>                 z <- maybeToList $ maybeSqrt z2
>                 return ((x, c), z)


> perimeters :: [Int]
> perimeters = map (\((a, c), _) -> 2*a + c) solutions'


> main' = print $ zip solutions' $ takeWhile (<= 10000000) perimeters


but it takes too long. let's "cheat"

Wolfram Alpha also says that there is a solution for

4x^2 - (x + 1)^2 = c^2 ; x > 0; c > 0 :

> x1 n = round $ (1 / 3) * (tmn + tpn + 1)
>     where tmn = (7 - 4 * sqrt 3) ** n
>           tpn = (7 + 4 * sqrt 3) ** n

Okay, I could do it by hand, but why? also

4x^2 - (x + 1)^2 = c^2 ; x > 0; c > 0 :

> x2 n = round $ (1 / 3) * ( 2 * tmn + sqrt 3 * tmn + 2 * tpn - sqrt 3 * tpn - 1)
>     where tmn = (7 - 4 * sqrt 3) ** n
>           tpn = (7 + 4 * sqrt 3) ** n

so we can say

> solutions1 = do n <- [1..]
>                 let x = x1 n
>                 let z2 = getZ2 x (x + 1)
>                 let z = floor $ sqrt $ fromIntegral z2
>                 return (x, x + 1)

> solutions2 = do n <- [2..]
>                 let x = x2 n
>                 let z2 = getZ2 x (x - 1)
>                 let z = floor $ sqrt $ fromIntegral z2
>                 return (x, x - 1)

> getPerimeter (a, c) = a + a + c

> solutions = let fil = (<= 1000000000) . getPerimeter in
>             takeWhile fil solutions1 ++ takeWhile fil solutions2