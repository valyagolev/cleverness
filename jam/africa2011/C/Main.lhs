
Solution

> module Main where

> import Data.Ratio
> import Data.List (sortBy)
> import Data.Ord (comparing)
> import Text.Printf (printf)
> import Debug.Trace (trace)

> import Control.Monad (replicateM_, replicateM, forM_)

> --trace a b = b

> data Message = Message { position :: Integer, time :: Integer }
>      deriving (Show)

> data Timeline = Timeline { msgs :: [Message] }
>      deriving (Show)

> data Range = Range { moment :: Integer, from :: Rational, to :: Rational }
>      deriving (Show)

> timeline :: [Message] -> Timeline
> timeline = Timeline . sortBy (comparing time)

> deltas :: (Num b) => (a -> b) -> [a] -> [b]
> deltas f xs = let vs = map f xs
>               in zipWith ((abs .) . (-)) vs (tail vs)

> ranges :: Rational -> Timeline -> [Range]
> ranges d (Timeline (m:ms)) = let r = rangeFor d m
>                                  rs = r : zipWith (nextRange d) ms rs
>                              in rs

> rangeFor :: Rational -> Message -> Range
> rangeFor d (Message p t) = Range t (fromIntegral p - d) (fromIntegral p + d)

> nextRange :: Rational -> Message -> Range -> Range
> nextRange d m lr = let frr = rangeFor d m
>                        abl = rangeExtend (time m - moment lr) lr
>                    in intersectRanges frr abl

> rangeExtend :: Integer -> Range -> Range
> rangeExtend dt (Range m f t) = Range (m + dt) (f - fromIntegral dt) (t + fromIntegral dt)

> intersectRanges :: Range -> Range -> Range
> intersectRanges (Range m1 f1 t1) (Range m2 f2 t2) | m1 == m2 =
>                   Range m1 (f1 `max` f2) (t1 `min` t2)
  
> appreciateD :: Timeline -> Rational -> Ordering
> appreciateD t d = let rs = ranges d t
>                       lower = any invalidRange rs
>                       higher = all bigRange rs
>                   in case (lower, higher) of
>                           (True, _) -> LT
>                           (_, True) -> GT
>                           _ -> EQ

> eps = 10 ** (-10)

> invalidRange :: Range -> Bool
> invalidRange (Range _ f t) = fromRational (f - t) > eps

> bigRange :: Range -> Bool
> bigRange (Range _ f t) = fromRational (t - f) > eps

> maxD :: Timeline -> Rational
> maxD (Timeline ms) = (maximum $ deltas position ms) % 2 

> minD :: Timeline -> Rational
> minD (Timeline ms) = (maximum $ zipWith (-) (deltas position ms) (deltas time ms)) % 2


> search :: Timeline -> Rational
> search (Timeline [_]) = 0
> search tl = let lower = minD tl
>                 higher = maxD tl
>             in binarySearch (appreciateD tl) lower higher

> --binarySearch :: (Fractional a) => (a -> Ordering) -> a -> a -> a
> --binarySearch ap l h | trace ("binarySearch " ++ (show (ourFloat l, ourFloat h))) False = undefined
> binarySearch ap l h = let cur = (l + h) / 2
>                       in if (abs $ fromRational $ l - h) < eps
>                          then cur
>                          else case ap cur of 
>                                 LT -> binarySearch ap cur h
>                                 GT -> binarySearch ap l cur
>                                 EQ -> cur
               

> tl1 = timeline [Message 7 2, Message 20 3, Message 0 11]
> tl2 = timeline [Message 6 5, Message 6 3]
> tl3 = timeline [Message 5 3, Message 2 1, Message 9 4, Message 7 2]

> readMessage :: IO Message
> readMessage = do lt <- getLine
>                  let (f, (' ':s)) = span (/=' ') lt
>                  return $ Message (read f) (read s)

> readTimeline :: IO Timeline
> readTimeline = do n <- readLn
>                   ms <- replicateM n readMessage
>                   return (timeline ms)

> solveCase :: Int -> IO ()
> solveCase n = do tl <- readTimeline
>                  putStr $ "Case #" ++ (show n) ++ ": " ++ ourFloat (search tl) ++ "\n"

> ourFloat :: Rational -> String
> ourFloat f = let s = printf "%.10f" (fromRational f :: Double)
>              in s
  
> main :: IO ()
> main = do c <- readLn
>           forM_ [1..c] solveCase

