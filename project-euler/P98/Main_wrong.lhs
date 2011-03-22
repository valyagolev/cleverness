Okay, I wasn't right at first since I interpreted problem not exactly right
Disregard this


> module P98.Main where

> import Data.List (sortBy, sort, elemIndices, subsequences, groupBy, nub)
> import Data.Ord (comparing)
> import Data.List.Split (splitOn)
> import Control.Monad (guard)


Let's start with a notion of *permutation*:

Permutation is a tuple... okay, it is a list of numbers.

> data Permutation = Permutation [Int]
>                  deriving (Show, Eq)

We can use every number as a *new index* for an element of another set:

> permute :: Permutation -> [b] -> [b]
> permute (Permutation d) l = map snd $ sortBy (comparing fst) $ zip d l


We will need to generate a permutations from two values, like "CARE" and "RACE".
There actually can be arbitrary number of permutations (eg, two for "AA" and "AA": identity and reverse). See `prop_example_perm{1,2}` in Tests.hs

"CARE" "RACE" : 2

(2, 1, 0, 3)

"ARE" | "RAE" : 1

(1, 0, 2)
       _

"RE" | "RE"

(0, 1)
    _


> permutations :: (Show b, Eq b) => [b] -> [b] -> [Permutation]
> permutations l1 l2 = map Permutation $ p_i l1 l2
>      where p_i [e1]    [e2] | e1 == e2 = [[0]]
>            p_i (h1:t1) l2              = do ins <- elemIndices h1 l2
>                                             perm <- p_i t1 (delete_index ins l2)
>                                             let np = map
>                                                      (\e -> if e >= ins
>                                                             then e + 1
>                                                             else e)
>                                                      perm
>                                             guard $ not $ ins `elem` np
>                                             return (ins : np)



> delete_index :: Int -> [a] -> [a]
> delete_index i l = take i l ++ drop (i + 1) l


That does the job.


Now to the problem. We need to find all anagram pairs and then all anagram square pairs.

We can first find all anagram pairs and then try to find good squares for them.

> main :: IO ()
> main = do l <- getLine 
>           let words = splitOn "," l
>           let rwords = map (init . tail) words                
>           let pairs = get_anagram_pairs rwords
>           let sorted_p = sortBy (comparing ((0-) . length . fst) ) pairs
>           let by_l = groupBy (\a b -> (length $ fst a) == (length $ fst b)) sorted_p
>           let sols = get_solutions $ concat by_l
>           mapM_ (putStrLn . show) sols
>   --        let (a, b) = head $ head by_l -- I believe!
>     --      let perms = permutations a b
>       --    let sqs = find_sqs $ head perms
>         --  putStrLn $ show (a, b)
> --          putStrLn $ show perms
>   --        putStrLn $ show sqs

> -- get_solutions :: [(String, String)] -> [(Int, Int)]
> get_solutions l = do (p1, p2) <- l
>                      p <- permutations p1 p2                       
>                      s <- find_sqs p
>                      return (s, p, (p1, p2))


> relevant_sqs :: Permutation -> [Int]
> relevant_sqs (Permutation l) = let max_i = 10 ^ length l
>                                    min_i = 10 ^ (length l - 1)
>                                    allsqs = [ n*n | n <- [1..] ]
>                                    in takeWhile (<max_i) $ dropWhile (<min_i) allsqs

> is_square n = sq * sq == n
>     where sq = floor $ sqrt $ (fromIntegral n::Double)

> find_sqs :: Permutation -> [(Int, Int)]
> find_sqs p = do sq <- relevant_sqs p
>                 let perm = read $ permute p (show sq)
>                 guard $ length (show sq) == length (show perm)
>                 guard $ is_square perm
>                 guard $ nub (show sq) == (show sq) 
>                 return (sq, perm)

> get_anagram_pairs :: [String] -> [(String, String)]
> get_anagram_pairs ws = [ (an1, an2) | ans <- anss, an1 <- ans, an2 <- ans, an1 < an2 ]
>           where anss = get_anagrams ws

> get_anagrams :: [String] -> [[String]]
> get_anagrams ws = map (map snd) $ filter ((>= 2) . length) groups
>           where nfrms = zip (map sort ws) ws
>                 nfrms_sorted = sortBy (comparing fst) nfrms
>                 groups = groupBy (\a b -> (fst a) == (fst b)) nfrms_sorted
