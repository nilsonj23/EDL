data Arvore = Folha Int | Galho Arvore Arvore
    deriving Show

a1 = Galho (Galho (Folha 10) (Folha 20)) (Galho (Folha 30) (Folha 40))
a2 = Galho (Galho (Galho (Folha 5) (Folha 15)) (Folha 25)) (Folha 35)
a3 = Galho (Folha 100) (Galho (Galho (Folha 120) (Folha 140)) (Galho (Folha 160) (Folha 180)))

folhas :: Arvore -> Int
folhas (Folha i)   = 1
folhas (Galho e d) = (folhas e) + (folhas d)

altura :: Arvore -> Int
altura (Folha i)   = 1
altura (Galho e d) = if (altura e > altura d) then (altura e + 1)
                        else (altura d + 1)

espelho :: Arvore -> Arvore
espelho (Folha i)   = (Folha i)
espelho (Galho e d) = Galho (espelho d) (espelho e)

soma :: Arvore -> Int
soma (Folha i)   = i
soma (Galho e d) = (soma e) + (soma d)

dobra :: Arvore -> Arvore
dobra (Folha i)   = (Folha (2*i))
dobra (Galho e d) = Galho (dobra e) (dobra d)

possui :: Arvore -> Int -> Bool
possui (Folha i) num   = i == num
possui (Galho e d) num = (possui e num) || (possui d num)

maximo :: Arvore -> Int
maximo (Folha i)   = i
maximo (Galho e d) = if (maximo e > maximo d) then (maximo e)
                        else (maximo d)

insere :: Arvore -> Int -> Arvore
insere (Folha i) num   = if (i == num) then Galho (Folha num) (Folha i)
                            else Galho (Folha i) (Folha num)
insere (Galho e d) num = Galho e (insere d num)

main = putStrLn("Arvore 1 = " ++ (show a1) ++ "\n" ++
                "Folhas = " ++ (show(folhas a1)) ++ "\n" ++
                "Altura = " ++ (show(altura a1)) ++ "\n" ++
                "Espelho = " ++ (show(espelho a1)) ++ "\n" ++
                "Soma = " ++ (show(soma a1)) ++ "\n" ++
                "Dobro = " ++ (show(dobra a1)) ++ "\n" ++
                "Possui = " ++ (show(possui a1 30)) ++ "\n" ++
                "Maximo = " ++ (show(maximo a1)) ++ "\n" ++
                "insere = " ++ (show(insere a1 15)) ++ "\n")