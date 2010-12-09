data StrangeNum = StrangeNum { integer_part :: Int, under_root_part :: Int }
     deriving (Eq, Ord, Show)

instance Num StrangeNum where
         a + b
           | under_root_part a == 0 && under_root_part b == 0 = fromIntegral (integer_part a + integer_part b)
           | under_root_part b == 0 = StrangeNum { integer_part = (integer_part a + integer_part b),
                                                   under_root_part = under_root_part a }
           | otherwise = error $ "(a = " ++ (show a) ++ ") + (b = " ++ (show b) ++ " )"
         a - b
           | under_root_part a == 0 && under_root_part b == 0 = fromIntegral (integer_part a - integer_part b)
           | under_root_part b == 0 = StrangeNum { integer_part = (integer_part a - integer_part b),
                                                   under_root_part = under_root_part a }
           | otherwise = error $ "(a = " ++ (show a) ++ ") - (b = " ++ (show b) ++ " )"
         fromInteger n = StrangeNum { integer_part = fromIntegral n, under_root_part = 0 }

instance Fractional StrangeNum where
         a / b
           | b == fromIntegral 1 = a
           | otherwise = error $ "(a = " ++ (show a) ++ ") / (b = " ++ (show b) ++ " )"

isq :: (Integral b, Integral a) => a -> b
isq n = floor $ sqrt $ (fromIntegral n::Double)

get_integer_part :: StrangeNum -> Int
get_integer_part st = integer_part st + isq (under_root_part st)

strange_sqrt :: Int -> StrangeNum
strange_sqrt n = StrangeNum { integer_part = 0, under_root_part = n } 

divide_num :: StrangeNum -> (Int, StrangeNum)
divide_num n = (int_part, rat_part)
           where int_part = get_integer_part n
                 rat_part = n - (fromIntegral int_part)

first_step :: Int -> (Int, StrangeNum)
first_step n = divide_num (strange_sqrt n)

next_step r = divide_num r'
          where r' = 1 / r

steps n 0 = [first_step n]
steps n m = next_step r : sts
      where sts = steps n (m - 1)
            (_, r) = head sts

delta = 0.00000001

too_close a b = abs (a - b) <= delta
            
extract_period n = (i0, period)
            where sts = steps n 20
                  (i0, _) = last sts
                  ((i1, r1):als) = reverse $ init sts
                  period = (i1, r1) : takeWhile (not . (too_close r1) . snd) als

period_len n = length (snd $ extract_period n)
                  
               