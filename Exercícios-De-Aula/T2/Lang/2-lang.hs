data Exp = Num Float
         | Mul Exp Exp
         | Div Exp Exp 
         deriving Show

e0 = Mul (Num 1) (Num 2)
e1 = Mul (Mul (Mul (Num 1) (Num 2)) (Num 3)) (Num 4)
e2 = Mul (Num 1) (Mul (Num 2) (Div (Num 3) (Num 4)))
e3 = Div (Div (Div (Num 1) (Num 2)) (Num 3)) (Num 4)
e4 = Div (Num 1) (Div (Num 2) (Div (Num 3) (Num 4)))
e5 = Div (Div (Num 1) (Num 2)) (Div (Num 3) (Num 4))

avalia::Exp -> Float 
avalia (Num v) = v
avalia (Mul a b) = (avalia a) * (avalia b)
avalia (Div a b) = (avalia a) / (avalia b)

avalia' :: Exp -> Exp
avalia' (Num v) = (Num v)
avalia' (Mul (Num a) (Num b)) = Num (a * b)
avalia' (Div (Num a) (Num b)) = Num (a / b)
avalia' (Mul a b)             = (avalia' (Mul (avalia' a) (avalia' b)))
avalia' (Div a b)             = (avalia' (Div (avalia' a) (avalia' b)))

main = putStrLn("Avalia = " ++ (show (avalia e1)) ++ "\n" ++
                "Avalia' = " ++ (show(avalia' e0)) ++ "\n" )