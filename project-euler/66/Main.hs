import Data.List
import Data.Maybe

pairs_to_list ((a, b):ps) = a : b : pairs_to_list ps
pairs_to_list _           = []

our_ds = filter (not . is_square) [1..100]

get_min_x max_k d = find (is_solution d) [1..max_k]
          where xs = pairs_to_list [ (k*d - 1, k*d + 1) | k <- [1..max_k] ]

is_solution d x = is_square y2
            where y2 = (x^2 - 1) / d
            
squares = map (\x -> x*x) [1..]
is_square n = n == (head $ dropWhile (<n) squares)

ct = length . filter isNothing

