Here go some functions common for project square puzzles

> module ProjectEuler (maybeSqrt, isSquare) where

> import Data.Maybe (isJust)

> isSquare :: Int -> Bool
> isSquare = isJust . maybeSqrt

> maybeSqrt :: Int -> Maybe Int
> maybeSqrt n = if sq * sq == n
>               then Just sq
>               else Nothing
>     where sq = floor $ sqrt $ (fromIntegral n::Double)