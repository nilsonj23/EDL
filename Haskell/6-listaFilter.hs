-- FILTER
data Lista = Vazia | Par Int Lista
  deriving Show

x = (Par 10 (Par 20 (Par 30 (Par 40 Vazia))))

filter' :: (Int -> Bool) -> Lista -> Lista
filter' f Vazia     = Vazia
filter' f (Par h t) = if f h then Par h (filter' f t)
                      else filter' f t

--maior20 :: Int -> Bool
--maior20 x = x > 20

soma :: Lista -> Int
soma l =
  case l of
    Vazia   -> 0
    Par h t -> h + (soma t)

maior :: Lista -> Int
maior l =
  case l of
    Vazia -> 0
    Par h t -> let m = maior t in 
                if h > m then h
                else m

ehpar :: Int -> Bool
ehpar v = ((mod v 2) == 0)

--main = print(filter' (maior20 x))
--main = print(maior x)
--main = print(soma x)
main = print (filter' ehpar x)
