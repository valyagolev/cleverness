import Data.Maybe (maybeToList)
import Data.List ((\\))
import Control.Monad (guard)

-- a + b + c = P
-- a * b = S / 2
-- r = 2S / P
--

isqrt n = let s = truncate $ sqrt $ fromIntegral n in
	  if s*s == n then Just n	
	  	      else Nothing
		

triaks_for_n n = do a <- [3..n]
		    let min_b = a `max` 4
		    let a2 = a*a
		    b <- [a..(n - a - 5)]
		    let c = n - a - b
		    guard $ a2 + b^2 == c^2
		    return (a, b, c)

triaks_for_n_cs n cs = do c <- takeWhile (< (n - 6)) cs
			  let max_a = ((n - c) `div` 2) `min` (n - c - 4)
			  a <- [3..max_a]
			  let b = n - c - a
			  guard $ a^2 + b^2 == c^2
			  return (a, b, c)

triaks_before max_n = triaks_iter 12 [5..max_n - 7]
	where triaks_iter n cs | n > max_n = []
	      triaks_iter n cs = let ans = triaks_for_n_cs n cs
	      			     new_cs = cs \\ map (\(_, _, c) -> c) ans
				 in (n, ans) : triaks_iter (n + 2) new_cs
	      			


only_one_elem [a] = True
only_one_elem _ = False

main = print $ filter ((==1) . length . snd) $ triaks_before 200
