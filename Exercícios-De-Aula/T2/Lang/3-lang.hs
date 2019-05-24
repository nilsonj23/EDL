data Exp = Num Float | Add Exp Exp | Sub Exp Exp | Mult Exp Exp | Div Exp Exp
    deriving Show

e0 = Num 1
e1 = Sub (Add (Num 1) (Num 10)) (Num 20)
e2 = Add (Num 1) (Sub (Num 10) (Num 20))
e3 = Sub (Add (Num 5) (Num 5)) (Sub (Num 5) (Num 5)) 
e4 = Add (Mult (Add (Num 1) (Num 2)) (Num 3)) (Num 4)
e5 = Add (Num 1) (Add (Num 2) (Add (Num 3) (Num 4)))
e6 = Sub (Sub (Sub (Num 1) (Num 2)) (Num 3)) (Num 4)
e7 = Sub (Num 1) (Sub (Num 2) (Sub (Num 3) (Num 4)))
e8 = Sub (Sub (Num 1) (Num 2)) (Sub (Num 3) (Num 4))

avalia :: Exp -> Float
avalia (Num v) = v
avalia (Add a b) = avalia a + avalia b
avalia (Sub a b) = avalia a - avalia b
avalia (Mult a b) = avalia a * avalia b
avalia (Div a b) = avalia a / avalia b

avalia' :: Exp -> Exp
avalia' (Num v) = Num v
avalia' (Add (Num v1) (Num v2)) = Num (v1 + v2)
avalia' (Sub (Num v1) (Num v2)) = Num (v1 - v2)
avalia' (Mult (Num v1) (Num v2)) = Num (v1 * v2)
avalia' (Div (Num v1) (Num v2)) = Num (v1 / v2)
avalia' (Add a b) = avalia' (Add (avalia' a) (avalia' b))
avalia' (Sub a b) = avalia' (Sub (avalia' a) (avalia' b))
avalia' (Mult a b) = avalia' (Mult (avalia' a) (avalia' b))
avalia' (Div a b) = avalia' (Div (avalia' a) (avalia' b))

main = putStrLn("Avalia = " ++ (show (avalia e1)) ++ "\n" ++
                "Avalia' = " ++ (show(avalia' e0)) ++ "\n" )