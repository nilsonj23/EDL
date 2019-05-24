data Arvore = Folha | Galho Int Arvore Arvore
    deriving Show

a1 = Galho 10 (Galho 20 Folha Folha) (Galho 30 Folha Folha)

folhas :: Arvore -> Int
folhas Folha        = 1
folhas (Galho _ e d) = (folhas e) + (folhas d)

altura :: Arvore -> Int
altura Folha         = 1
altura (Galho _ e d) = if (altura e) > (altura d) then (altura e + 1)
                        else (altura d + 1)

espelho :: Arvore -> Arvore
espelho Folha         = Folha
espelho (Galho i e d) = Galho i (espelho d) (espelho e)

soma :: Arvore -> Int
soma Folha         = 0
soma (Galho i e d) = i + (soma e) + (soma d)

dobra :: Arvore -> Arvore
dobra Folha         = Folha
dobra (Galho i e d) = Galho (i*2) (dobra e) (dobra d)

possui :: Arvore -> Int -> Bool
possui Folha num       = False
possui (Galho i e d) num = if (i == num) then True
                            else (possui e num) || (possui d num)

maximo :: Arvore -> Int
maximo Folha             = 0
maximo (Galho i e d) =  if (i > maximo d) then i
                                else (maximo d)

insere :: Arvore -> Int -> Arvore
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
                "insere = " ++ (show(insere a1 15)) ++ "\n")