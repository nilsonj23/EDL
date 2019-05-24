data Arvore a = Folha | Galho a (Arvore a) (Arvore a)
    deriving Show

a1 = Galho 10 (Galho 20 Folha Folha) (Galho 30 Folha Folha)
a2 = Galho 5 (Galho 15 (Galho 25 Folha Folha) Folha) Folha
a3 = Galho 100 Folha (Galho 120 (Galho 130 Folha Folha) (Galho 140 Folha Folha))

folhas :: Arvore a -> Int
folhas Folha         = 1
folhas (Galho _ e d) = (folhas e) + (folhas d)

altura :: Arvore a -> Int
altura Folha         = 1
altura (Galho _ e d) = if (altura e > altura d) then (altura e + 1)
                        else (altura d + 1)

espelho :: Arvore a -> Arvore a
espelho Folha         = Folha
espelho (Galho i e d) = Galho i (espelho d) (espelho e)

soma :: Num a => Arvore a -> a
soma Folha         = 0
soma (Galho i e d) = i + (soma e) + (soma d)

dobra :: Num a => Arvore a -> Arvore a
dobra Folha         = Folha
dobra (Galho i e d) = Galho (2*i) (dobra e) (dobra d)

possui :: Ord a => Arvore a -> a -> Bool
possui Folha num         = False
possui (Galho i e d) num = if (i == num) then True
                          else (possui e num) || (possui d num)

maximo :: Num a => Ord a => Arvore a -> a
maximo Folha         = 0
maximo (Galho i e d) = if (i > maximo d) then i
                        else (maximo d)

insere :: Num a => Ord a => Arvore a -> a -> Arvore a
insere Folha num         = Galho num Folha Folha
insere (Galho i e d) num = if (num > i) then Galho i e (insere d num)
                            else Galho i (insere e num) d

main = putStrLn("Arvore 1 = " ++ (show a1) ++ "\n" ++
                "Folhas = " ++ (show(folhas a1)) ++ "\n" ++
                "Altura = " ++ (show(altura a1)) ++ "\n" ++
                "Espelho = " ++ (show(espelho a1)) ++ "\n" ++
                "Soma = " ++ (show(soma a1)) ++ "\n" ++
                "Dobro = " ++ (show(dobra a1)) ++ "\n" ++
                "Possui = " ++ (show(possui a1 30)) ++ "\n" ++
                "Maximo = " ++ (show(maximo a1)) ++ "\n" ++
                "insere = " ++ (show(insere a1 5)) ++ "\n")