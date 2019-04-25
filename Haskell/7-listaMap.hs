-- MAP
--data Lista = Vazia | Par Int Lista
data Lista a = Vazia | Par a (Lista a)
  deriving Show

x = (Par 10 (Par 20 (Par 30 (Par 40 Vazia))))

--map' :: (Int -> Int) -> Lista -> Lista
map' :: (a -> b) -> (Lista a) -> (Lista b)
map' f Vazia     = Vazia
map' f (Par h t) = Par (f h) (map' f t)

soma10 :: Int -> Int
soma10 x = x + 10

dobra :: Int -> Int 
dobra v = 2 * v

a = map' soma10 x
b = map' dobra x

main = putStrLn("\n a = " ++ (show a) ++ "\n b = " ++ (show b))