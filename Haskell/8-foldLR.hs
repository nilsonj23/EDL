data Lista a = Vazia | Par a (Lista a)
  deriving (Show)

l :: Lista Int
l = Par 1 (Par 2 (Par 3 (Par 4 Vazia)))

--foldl' :: (Int -> Int -> Int) -> Int -> Lista -> Int
foldl' :: (b->a->b) -> b -> Lista a -> b

-- FOLDL
foldl' f acc l = case l of
                Vazia        -> acc
                (Par h t) -> foldl' f (f acc h) t


-- FOLDR
foldr' :: (a->b->b) -> b -> Lista a -> b
foldr' f acc l = case l of
                Vazia        -> acc
                (Par h t) -> f h (foldr' f acc t)

f3 :: Int -> Int -> Int
f3 x y = x - y

soma v1 v2 = v1 + v2
maior v1 v2 = if v1 > v2 then v1 else v2

a = (foldr' soma 0 l)
b = (foldr' maior 0 l)
subl = foldl' f3 0 l
subr = foldr' f3 0 l

main = putStrLn("\n soma = " ++ (show a) ++ "\n maior = " ++ (show b) ++ "\n subl = " ++ (show subl) ++ "\n subr = " ++ (show subr))