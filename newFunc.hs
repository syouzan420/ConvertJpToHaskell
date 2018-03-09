module NewFunc (map') where

map' :: [a] -> (a -> b) -> [b]
map' x y = map y x


xx :: Num a => a -> a -> a
xx x y = (*) y x
