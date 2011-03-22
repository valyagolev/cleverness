> module P98.Main where

> import Data.List (sortBy, sort, elemIndex, subsequences, groupBy, nub)
> import Data.Ord (comparing)
> import Data.Maybe (maybeToList)
> import Data.List.Split (splitOn)
> import Control.Monad (guard, liftM)


> data Permutation = Permutation [Int]
>                  deriving (Show, Eq)

"CARE" "RACE" : 2 1 0 3

"AA" "AA" : 0 0

We can use every number as a *new index* for an element of another set:

> permute :: Permutation -> [b] -> [b]
> permute (Permutation (i:d)) l = (l !! i) : permute (Permutation d) l
> permute _ _ = []

"CARE" 2 1 0 3
R : "CARE" 1 0 3
RA : "CARE" 0 3

"AA" 0 0
A : "AA" 0
AA : "" []


"CARE" "RACE"

> permutation :: (Eq b) => [b] -> [b] -> Maybe Permutation
> permutation l1 l2 = Permutation `liftM` do_p l1 l2
>           where do_p l1 (h2:t2) = do i <- elemIndex h2 l1
>                                      p <- do_p l1 t2
>                                      return (i : p)
>                 do_p l1 []      = Just []
                       

Not let's solve it

> main :: IO ()
> main = do l <- getLine 
>           let words = splitOn "," l
>           let rwords = map (init . tail) words                
>           let pairs = get_anagram_pairs rwords
>           let sorted_p = sortBy (comparing ((0-) . length . fst) ) pairs
>           let by_l = groupBy (\a b -> (length $ fst a) == (length $ fst b)) sorted_p
>           let sols = get_solutions $ concat by_l
>           mapM_ (putStrLn . show) sols

> get_solutions l = do (p1, p2) <- l
>                      p <- maybeToList $ permutation p1 p2                       
>                      s <- find_sqs p
>                      return (s, p, (p1, p2))

> find_sqs :: Permutation -> [(Int, Int)]
> find_sqs p = do sq <- relevant_sqs p
>                 let perm = read $ permute p (show sq)
>                 guard $ permutation (show sq) (show perm) == Just p
>                 guard $ is_square perm
> --                guard $ nub (show sq) == (show sq) 
>                 return (sq, perm)



> is_square n = sq * sq == n
>     where sq = floor $ sqrt $ (fromIntegral n::Double)

> relevant_sqs :: Permutation -> [Int]
> relevant_sqs (Permutation l) = let max_i = 10 ^ length l
>                                    min_i = 10 ^ (length l - 1)
>                                    allsqs = [ n*n | n <- [1..] ]
>                                    in takeWhile (<max_i) $ dropWhile (<min_i) allsqs



> get_anagram_pairs :: [String] -> [(String, String)]
> get_anagram_pairs ws = [ (an1, an2) | ans <- anss, an1 <- ans, an2 <- ans, an1 < an2 ]
>           where anss = get_anagrams ws

> get_anagrams :: [String] -> [[String]]
> get_anagrams ws = map (map snd) $ filter ((>= 2) . length) groups
>           where nfrms = zip (map sort ws) ws
>                 nfrms_sorted = sortBy (comparing fst) nfrms
>                 groups = groupBy (\a b -> (fst a) == (fst b)) nfrms_sorted
