import Debug.Trace
import Test.HUnit

fold' f acc [] = acc
fold' f acc (x:xs) = trace ("Calling fold with x = " ++ show x ++ " and xs = " ++ show xs) $ (f x (fold' f acc xs))
