module newFunc (map') where

map' :: [a] -> (a -> b) -> [b]
map' x y = map y x


