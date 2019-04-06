type Autores = [String]
type Musica  = (String, Int, Int)

bandas :: [Autores]
bandas = [ ["Gilberto Gil"],
           ["Victor","Leo"],
           ["Gonzagao"],
           ["Claudinho","Bochecha"] ]

musicas :: [Musica]
musicas = [ ("Aquele Abraco", 1, 100),
            ("Esperando na Janela", 1, 150),
            ("Borboletas", 2, 120),
            ("Asa Branca", 3, 120),
            ("Assum Preto", 3, 140),
            ("Vem Morena", 3, 200),
            ("Nosso Sonho", 4, 150),
            ("Quero te Encontrar", 4, 100) ]

f1 (nomeM, _, _) = nomeM
f2 (_, _, tempo) = tempo
f3 tempo = tempo >= 120
f4 maior atual = if maior > atual then maior else atual 
f5 (_, _, tempo) = tempo >= 120

-- O nome das músicas
ex1 = print(map f1 musicas)

-- Músicas com >= 2min
ex2 = print(filter f3 (map f2 musicas))

-- Maior duração
ex3 = print(foldr f4 0 (map f2 musicas))

-- Nomes com >= 2min
ex4 = print(map f1 (filter f5 musicas))

