

readRoman :: String -> Int
readRoman []           = 0
readRoman ('I':'V':cs) = 4 + readRoman cs
readRoman ('I':'X':cs) = 9 + readRoman cs
readRoman ('X':'L':cs) = 40 + readRoman cs
readRoman ('X':'C':cs) = 90 + readRoman cs
readRoman ('C':'D':cs) = 400 + readRoman cs
readRoman ('C':'M':cs) = 900 + readRoman cs
readRoman ('I':cs)     = 1 + readRoman cs
readRoman ('V':cs)     = 5 + readRoman cs
readRoman ('X':cs)     = 10 + readRoman cs
readRoman ('L':cs)     = 50 + readRoman cs
readRoman ('C':cs)     = 100 + readRoman cs
readRoman ('D':cs)     = 500 + readRoman cs
readRoman ('M':cs)     = 1000 + readRoman cs

rules = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"),
         (100, "C"), (90, "XC"), (50, "L"), (40, "XL"),
         (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]

bestRoman :: Int -> String
bestRoman 0 = ""
bestRoman n = c ++ bestRoman (n - m)
    where (m, c) = head $ filter ((<= n) . fst) rules


solve s = sum diffs
      where ls = lines s
            nums = map (bestRoman . readRoman) ls
            len_pairs = zip (map length ls) (map length nums)
            diffs = map (\(a, b) -> (a - b)) len_pairs

main :: IO()
main = interact $ show . solve