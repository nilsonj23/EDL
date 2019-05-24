data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp 
         deriving Show

--LISTA DE EXEMPLOS
--1
ex0 = Num 1
--1 + 10 - 20
ex1 = Sub (Add (Num 1) (Num 10)) (Num 20)
--1 + (10 - 20)
ex2 = Add (Num 1) (Sub (Num 10) (Num 20))
--(5 + 5) - (5 - 5)
ex3 = Sub (Add (Num 5) (Num 5)) (Sub (Num 5) (Num 5))

e0 = Add (Num 1) (Num 2)
e1 = Add (Add (Add (Num 1) (Num 2)) (Num 3)) (Num 4)
e2 = Add (Num 1) (Add (Num 2) (Add (Num 3) (Num 4)))
e3 = Sub (Sub (Sub (Num 1) (Num 2)) (Num 3)) (Num 4)
e4 = Sub (Num 1) (Sub (Num 2) (Sub (Num 3) (Num 4)))
e5 = Sub (Sub (Num 1) (Num 2)) (Sub (Num 3) (Num 4))

avalia::Exp -> Int 
avalia (Num v) = v
avalia (Add a b) = (avalia a) + (avalia b)
avalia (Sub a b) = (avalia a) - (avalia b)

avalia' :: Exp -> Exp
avalia' (Num v) = (Num v)
avalia' (Add (Num a) (Num b)) = Num (a + b)
avalia' (Sub (Num a) (Num b)) = Num (a - b)
avalia' (Add a b)             = (avalia' (Add (avalia' a) (avalia' b)))
avalia' (Sub a b)             = (avalia' (Sub (avalia' a) (avalia' b)))


main = putStrLn("Avalia = " ++ (show (avalia e1)) ++ "\n" ++
                "Avalia' = " ++ (show(avalia' e0)) ++ "\n" )