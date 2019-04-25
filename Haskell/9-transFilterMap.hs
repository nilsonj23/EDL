data Lista a = Vazio | Par a (Lista a)
  deriving (Show)

l :: Lista Int
l = Par 1 (Par 2 (Par 3 (Par 4 Vazio)))

-- FOLDL
foldr' :: (a->b->b) -> b -> Lista a -> b
foldr' f v l = case l of
                Vazio        -> v
                (Par h tail) -> (f h (foldr' f v tail))
f3 x y = x + y
v1 = foldr' f3 0 l

-- FILTER
filter' :: (a->Bool) -> (Lista a) -> (Lista a)
filter' f l =
    foldr'
        (\new acc ->
            if (f new) then
                Par new acc
            else
                acc)
        Vazio
        l
a = filter' (\x -> (mod x 2 == 1)) l

-- MAP
map' :: (a->b) -> (Lista a) -> (Lista b)
map' f l =
    foldr'
        (\new acc -> Par (f new) acc)
        Vazio
        l
b = map' (\x -> (x*2)) l

main = putStrLn("\n filter = " ++ (show a) ++ "\n map = " ++ (show b))