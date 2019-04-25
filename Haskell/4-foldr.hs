-- FOLD (REDUCE)
a = foldr (+) 0 [10,20,30,40]
-- 100

f1 maior atual = 
  if atual > maior then atual
  else maior
b = foldr f1 0 [10,20,300,40]
-- 300

f2 atual cont =
  if atual == 10 then cont + 1 
  else cont
c = foldr f2 0 [10,20,300,10,40]
--2 

d = foldr (++) [] [[10,20,300], [10,40]]

main = putStrLn("\n a = " ++ (show a) ++ "\n b = " ++ (show b) ++ "\n c = " ++ (show c) ++ "\n d = " ++ (show d))

l :: [Int]
l = [1, 2, 3, 4]

f1 :: Int -> Int -> Int
f1 x y = (x + y)
m1 = foldr f1 0 l

f2 :: Int -> Int -> Int
f2 x y = (x * y)
m2 = foldr f2 1 l

--main = putStrLn("\n soma = " ++ (show m1) ++ "\n mult = " ++ (show m2))