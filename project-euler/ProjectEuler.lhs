Here go some functions common for project square puzzles

> module ProjectEuler where

> import Data.Maybe (isJust)


Squares and square roots:

> isSquare :: (Integral a) => a -> Bool
> isSquare = isJust . maybeSqrt

> maybeSqrt :: (Integral a) => a -> Maybe a
> maybeSqrt n = if sq * sq == n
>               then Just sq
>               else Nothing
>     where sq = intSqrt n

> intSqrt :: (Integral a) => a -> a
> intSqrt n = floor $ sqrt $ (fromIntegral n::Double)

Division:

> dividedBy :: (Integral a) => a -> a -> Bool
> a `dividedBy` b = a `mod` b == 0


Primes (naive):


> listOfPrimes :: (Integral a) => [a]
> listOfPrimes = 2 : filter isPrime [3..]
>        where isPrime n = not $ any (n `dividedBy`) $ takeWhile (<= intSqrt n) listOfPrimes
