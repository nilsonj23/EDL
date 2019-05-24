data Arvore = Folha | Galho Arvore Arvore
    deriving Show

a1 = Galho (Galho Folha Folha) (Galho Folha Folha)
a2 = Galho (Galho (Galho Folha Folha) Folha) Folha
a3 = Galho Folha (Galho (Galho Folha Folha) (Galho Folha Folha))

folhas :: Arvore -> Int
folhas Folha       = 1
folhas (Galho e d) = (folhas e) + (folhas d)

altura :: Arvore -> Int
altura Folha       =  1
altura (Galho e d) = if (folhas e) > (folhas d) then (1 + altura e)
                        else (1 + altura d)

espelho :: Arvore -> Arvore
espelho Folha       = Folha
espelho (Galho e d) = (Galho d e)


main = putStrLn("A1 = " ++ (show a1) ++ "\n" ++
                "A2 = " ++ (show a2) ++ "\n" ++
                "A3 = " ++ (show a3) ++ "\n" ++
                "Folhas = " ++ (show(folhas a1)) ++ "\n" ++
                "Altura = " ++ (show(altura a2)) ++ "\n" ++
                "Espelho = " ++ (show(espelho a3)))