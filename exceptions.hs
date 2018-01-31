--data Either a b = Left a |Right b

data Exception = DivideByZero | RudeWord | PoorHumor
type Exceptional a = Either Exception a

throw :: Exception -> Exceptional a
throw x = Left x

return :: a -> Exceptional a
return a = Right a

--new division operation that catches exception
(///) :: Int -> Int -> Exceptional Int
_ /// 0 = throw DivideByZero
a /// b = Main.return $ a `div` b
