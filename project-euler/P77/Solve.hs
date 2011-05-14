{-
sum of primes is a sum of prime factors of some integer

10 = sopfr(21) = sopfr(25) = sopfr(30) ...

we can enumerate integers: 1, 2, ... until find a recurring sopfr()

or not


-}

import Data.List
import Control.Monad
import Data.Ord 

import Data.Numbers.Primes (primes)

bigM = 75

primeSums 0 = [[]]
primeSums n = do s <- primeSums (n - 1)
                 p <- primes
                 return (p:s)

lPrimeSumsUntil = map primeSumsUntil [1..]

primeSumsUntil n = nub $ map sort $ primeSumsUntil' n

primeSumsUntil' 1 = map (:[]) $ takeWhile (<=bigM) primes
primeSumsUntil' n = do s <- lPrimeSumsUntil !! (n - 2)
                       let dt = bigM - sum s
                       p <- takeWhile (<=dt) primes 
                       return (p:s)
                        
allPrimeSumsUntil = concat $ takeWhile (not . null) $ lPrimeSumsUntil
allPrimeSumTotalsUntil = map sum $ allPrimeSumsUntil

primeSumsCountsUntil = sortBy (comparing snd) $ map (\xs -> (head xs, length xs)) $ group $ sort $ allPrimeSumTotalsUntil

main :: IO()
main = print $ primeSumsCountsUntil