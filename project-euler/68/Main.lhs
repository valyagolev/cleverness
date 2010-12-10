\documentclass{article}

%include polycode.fmt
%options ghci

\usepackage{amsmath}

\long\def\ignore#1{}

\begin{document}

\section{The problem}

This is a problem 68 from Project Euler.

\section{Definitions}

> import Data.List(delete, (\\))
> import Data.Maybe(maybeToList)
> import Control.Monad (guard)

Let's start with solving a problem for 3-gon ring.

> data Ring = Ring { outer :: [Int]
>                  , inner :: [Int] }
>                  deriving (Show, Eq)

> ring_size = 5

The ring is described in a strongly defined way,
so we need to "normalize" a ring - describe it from the lowest outer element.
If the order is wrong, we'll just rotate a ring $60\,^{\circ}$ clockwise
and call |normalize| again, recursively.

> normalize :: Ring -> Ring
> normalize r = if head (outer r) == minimum (outer r)
>               then r
>               else normalize $ Ring { outer = tail $ outer r ++ [head $ outer r]
>                                     , inner = tail $ inner r ++ [head $ inner r] }
            
Another concept in the problem description is a "description string".

> describe :: Ring -> [[Int]]
> describe r = describer 0
>        where describer i | i == ring_size = []
>                          | otherwise      = [ (outer r !! i)
>                                             , (inner r !! i)
>                                             , (inner r !! ((i + 1) `mod` ring_size)) ] :
>                                             describer (i + 1)

There are $6! = 720$ variants of rings for 3-gon rings without rotations. That can be just brute-forced. To help us in bruteforcing, we define a $describe^-1$ function.

> undescribe :: [[Int]] -> Maybe Ring
> undescribe []           = Just $ Ring { inner = [], outer = [] }
> undescribe ([o,i1,i2]:ns) = do partial <- undescribe ns
>                                new_inner <- case inner partial of
>                                                []       -> Just [i1, i2]
>                                                xs       | length xs == ring_size ->
>                                                                   if last xs == i1
>                                                                   then Just $ i1 : (init xs)
>                                                                   else Nothing
>                                                (ip1:xs) | ip1 == i2 -> Just (i1 : ip1 : xs)
>                                                _                    -> Nothing
>                                return $ Ring { outer = o : outer partial
>                                              , inner = new_inner }


The ring must be "magical": that is, sum of all its chunks must be constant:

> is_magical :: Ring -> Bool
> is_magical r = and $ map ((== sum (head chunks)) . sum) (tail chunks)
>      where chunks = describe r


Let's begin enumerating. Let's select "outer" numbers first:

> enumerate_rings' = enumerator [1..ring_size*2]
>      where enumerator [d] = [Ring [] [d]]
>            enumerator digits
>                     | length digits == ring_size*2 =
>                              do d <- filter (/= 10) $ filter (<= ring_size + 1) digits
>                                 r <- filter (and . map (> d) . outer) $ enumerator (d `delete` digits)
>                                 return $ r { outer = d : outer r }
>                     | length digits > ring_size =
>                              do d <- digits
>                                 r <- enumerator (d `delete` digits)
>                                 return $ r { outer = d : outer r }
>                     | otherwise =
>                              do d <- digits
>                                 r <- enumerator (d `delete` digits)
>                                 return $ r { inner = d : inner r }
>
>                                                      

Another approach is to generate by chunks:

> digits = [1..ring_size*2]

> get_first_chunk = do digit1 <- digits
>                      digit2 <- digit1 `delete` digits
>                      digit3 <- digits \\ [digit1, digit2]
>                      return [digit1, digit2, digit3]


> get_chunk :: [[Int]] -> [[Int]]
> get_chunk alr
>     | length alr == ring_size - 1 =
>                         let prev = last alr
>                             next = head alr
>                             s = sum next
>                             digit2 = last prev
>                             digit3 = head (tail next)
>                             digit1 = s - digit2 - digit3
>                             dav = digits \\ (digit2:digit3:(concat alr)) in
>                         if ([digit1] == dav) && (digit1 > head next)
>                         then [[digit1, digit2, digit3]]
>                         else []
>     | otherwise       = do let prev = last alr
>                            let next = head alr
>                            let s = sum prev
>                            let digit2 = last prev
>                            let dav = digit2 `delete` digits \\ (concat alr)
>                            digit1 <- filter (> head next) dav
>                            let digit3 = s - digit1 - digit2
>                            guard (digit3 `elem` (digit1 `delete` dav))
>                            return [digit1, digit2, digit3]

> enumerate_rings = do chunk <- get_first_chunk
>                      chunk2 <- get_chunk [chunk]
>                      chunk3 <- get_chunk [chunk, chunk2]
>                      chunk4 <- get_chunk [chunk, chunk2, chunk3]
>                      chunk5 <- get_chunk [chunk, chunk2, chunk3, chunk4]
>                      d <- maybeToList $ undescribe [chunk, chunk2, chunk3, chunk4, chunk5]
>                      return d


That's not very scalable, but works, and works fast. Long live Haskell!

> main :: IO()
> main = print $ concat $ map show $ concat $ maximum $ map describe enumerate_rings


Problem solved, the answer is \eval{main}.

\end{document}