
> module P187.Main where

> import Data.List (nub, sort)

> import ProjectEuler (listOfPrimes)

> import Data.Numbers.Primes (primes)


> composites' :: Int -> [Int]
> composites' max = do p1 <- primes
>                      p2 <- takeWhile (<= (max `div` p1)) $ dropWhile (< p1) primes
>                      return (p1 * p2)
>             where primes = takeWhile (<= (max `div` 2)) listOfPrimes

> composites :: Int -> [Int]
> composites = nub . composites'

a better solutioon?
                    
                    
> compositesZipper :: Int -> [Int] -> [Int]
> compositesZipper _ [] = []
> compositesZipper max primes = map (cur *) current_primes ++
>                               compositesZipper max next_primes
>                  where cur = head primes
>                        current_primes = takeWhile (<= (max `div` cur)) primes
>                        next_primes = tail current_primes

> composites'' :: Int -> [Int]
> composites'' max = compositesZipper max $ takeWhile (<= (max `div` 2)) listOfPrimes

just length please

> compositesZipperL :: Int -> [Int] -> Int
> compositesZipperL _ [] = 0
> compositesZipperL max primes = length current_primes +
>                                compositesZipperL max next_primes
>                   where cur = head primes
>                         current_primes = takeWhile (<= (max `div` cur)) primes
>                         next_primes = if current_primes == []
>                                       then []
>                                       else tail current_primes

> compositesL :: Int -> Int
> compositesL max = compositesZipperL max $ takeWhile (<= (max `div` 2)) primes


> main :: IO ()
> main = print $  compositesL 100000000

