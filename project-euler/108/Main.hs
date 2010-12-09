import Data.Ratio
import Data.Maybe
import Data.Ord
import Data.List

--import Control

is_solution :: (Integral t) => t -> t -> t -> Bool
is_solution n x y = 1 % x + 1 % y == 1 % n

get_y :: (Integral a) => a -> a -> Maybe a
get_y n x = if divides
             then Just (y_q `div` y_p)
             else Nothing
      where y_p = x - n
            y_q = x * n
            divides = y_q `mod` y_p == 0

can_solve n x = (x * n) `mod` (x - n) == 0
           

all_solutions n = can_solve n `filter` xs
              where median = 2 * n
                    xs = [(n+1)..median]


sol_count :: Integer -> Int
sol_count = length . all_solutions



sol_counts' :: [(Integer, Int)]
sol_counts' = take 200 $ zip ns (map sol_count ns)
            where ns = [10000,10000 + 2..510510]

max_here = (sol_counts', "====", maximumBy (comparing snd) sol_counts')
sol_counts = sol_counts'

solutions q1 q2 = 1 + q1 + q2 + 2*q1*q2
a_solutions v = v - 1

solutions3 q1 q2 q3 = solutions q12 q3
   where q12 = q1 + q2 + 2*q1*q2

check a b c = sol_count (2^a * 3^b * 5^c) == solutions3 a b c

sn :: [Int] -> Int
sn = foldr (\q2 r -> solutions (a_solutions r) q2) 1

val :: [Int] -> Integer
val = product . zipWith (^) the_prime_list

checkn :: [Int] -> Bool
checkn qs = sn qs == solcs qs
       where solcs qs = sol_count $ val qs


variants 1 = map (\a -> [a]) [1.. max_exp_for_ith 0]
variants n = [ v++[a] | v <- (variants (n-1)), a <- [0,1.. max_exp_for_ith (n-1)], a <= last v ]

data Variant = Variant { values :: [Int] }

instance Show Variant where
         show v = "Variant " ++ show (values v) ++ ", number " ++
                             show (realvalue v) ++ ", count " ++ show (solcount v) ++ "."

the_prime_list = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]

max_exp_for n =  last $ takeWhile (\x -> n^x < 510510) [1..]
max_exp_for_ith i = max_exp_for (the_prime_list !! i)
                             
solcount (Variant vs) = sn vs
realvalue (Variant vs) = val vs

tries n = filter ft $ take 100000 $ map Variant vs
       where vs = variants n
             ft v = solcount v > 4000000 && realvalue v < 9350130049860600


tries_s = sortBy (comparing realvalue) . tries              

tryall = concatMap tries [3..7]
main = print $ (ts, v)
     where ts = tryall
           v = ts
           --v = sortBy (comparing realvalue) ts
           

